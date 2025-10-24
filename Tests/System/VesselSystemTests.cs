using System;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using FluentAssertions;
using Microsoft.AspNetCore.Hosting;
using System.IO;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Newtonsoft.Json;
using Xunit;
using DDDSample1.Domain.Vessels;
using DDDSample1.Infrastructure;

namespace DDDSample1.Tests.System
{
    public class VesselSystemTests : IClassFixture<WebApplicationFactory<DDDSample1.Program>>
    {
        private readonly WebApplicationFactory<DDDSample1.Program> _factory;

        public VesselSystemTests()
        {
            _factory = new TestApplicationFactory();
        }

        [Fact]
        public async Task Vessel_Full_Workflow_via_HttpApi()
        {
            var client = _factory.CreateClient();

            // 0) Create a VesselType first (required dependency)
            Guid vesselTypeId;
            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<DDDSample1DbContext>();
                var vesselType = new VesselType("System Test Type", "For testing", 1000, 10, 20, 8);
                db.VesselTypes.Add(vesselType);
                await db.SaveChangesAsync();
                vesselTypeId = vesselType.Id.AsGuid();
            }

            // 1) Create Vessel
            var createDto = new CreatingVesselDto 
            { 
                ImoNumber = "IMO9999999", 
                Name = "System Test Vessel", 
                VesselTypeId = vesselTypeId,
                Owner = "System Test Owner",
                Operator = "System Test Operator"
            };
            var createJson = JsonConvert.SerializeObject(createDto);
            var createContent = new StringContent(createJson, Encoding.UTF8, "application/json");

            var createResponse = await client.PostAsync("/api/Vessels", createContent);
            createResponse.StatusCode.Should().Be(HttpStatusCode.Created);

            var createdBody = await createResponse.Content.ReadAsStringAsync();
            var created = JsonConvert.DeserializeObject<VesselDto>(createdBody);
            created.Should().NotBeNull();
            created.Id.Should().NotBe(Guid.Empty);
            created.ImoNumber.Should().Be(createDto.ImoNumber);
            created.Name.Should().Be(createDto.Name);
            created.Active.Should().BeTrue();

            // 2) Get by id
            var getResponse = await client.GetAsync($"/api/Vessels/{created.Id}");
            getResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var getBody = await getResponse.Content.ReadAsStringAsync();
            var got = JsonConvert.DeserializeObject<VesselDto>(getBody);
            got.Should().NotBeNull();
            got.Id.Should().Be(created.Id);

            // 3) Get by IMO number
            var getByImoResponse = await client.GetAsync($"/api/Vessels/imo/{created.ImoNumber}");
            getByImoResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var getByImoBody = await getByImoResponse.Content.ReadAsStringAsync();
            var gotByImo = JsonConvert.DeserializeObject<VesselDto>(getByImoBody);
            gotByImo.Should().NotBeNull();
            gotByImo.Id.Should().Be(created.Id);

            // 4) Update
            Guid newVesselTypeId;
            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<DDDSample1DbContext>();
                var newVesselType = new VesselType("Updated Type", "Updated for testing", 1100, 11, 21, 9);
                db.VesselTypes.Add(newVesselType);
                await db.SaveChangesAsync();
                newVesselTypeId = newVesselType.Id.AsGuid();
            }

            var updateDto = new UpdatingVesselDto 
            { 
                Name = "System Test Vessel Updated", 
                VesselTypeId = newVesselTypeId,
                Owner = "System Test Owner Updated",
                Operator = "System Test Operator Updated"
            };
            var updateJson = JsonConvert.SerializeObject(updateDto);
            var updateContent = new StringContent(updateJson, Encoding.UTF8, "application/json");
            var updateResponse = await client.PutAsync($"/api/Vessels/{created.Id}", updateContent);
            updateResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var updatedBody = await updateResponse.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<VesselDto>(updatedBody);
            updated.Should().NotBeNull();
            updated.Name.Should().Be(updateDto.Name);
            updated.Owner.Should().Be(updateDto.Owner);
            updated.Operator.Should().Be(updateDto.Operator);

            // 5) Get all and ensure contains
            var allResponse = await client.GetAsync("/api/Vessels");
            allResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var allBody = await allResponse.Content.ReadAsStringAsync();
            var all = JsonConvert.DeserializeObject<VesselDto[]>(allBody);
            all.Should().NotBeNull();
            all.Select(v => v.Id).Should().Contain(created.Id);

            // 6) Search by name
            var searchByName = await client.GetAsync("/api/Vessels/search/name?term=Updated");
            searchByName.StatusCode.Should().Be(HttpStatusCode.OK);
            var searchBody = await searchByName.Content.ReadAsStringAsync();
            var searchResults = JsonConvert.DeserializeObject<VesselDto[]>(searchBody);
            searchResults.Should().Contain(v => v.Id == created.Id);

            // 7) Search by owner
            var searchByOwner = await client.GetAsync("/api/Vessels/search/owner?term=System");
            searchByOwner.StatusCode.Should().Be(HttpStatusCode.OK);

            // 8) Search by operator
            var searchByOperator = await client.GetAsync("/api/Vessels/search/operator?term=Operator");
            searchByOperator.StatusCode.Should().Be(HttpStatusCode.OK);

            // 9) Soft delete (inactivate)
            var softDeleteResponse = await client.DeleteAsync($"/api/Vessels/{created.Id}");
            softDeleteResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var softBody = await softDeleteResponse.Content.ReadAsStringAsync();
            var softDto = JsonConvert.DeserializeObject<VesselDto>(softBody);
            softDto.Active.Should().BeFalse();

            // 10) Activate
            var activateResponse = await client.PostAsync($"/api/Vessels/{created.Id}/activate", null);
            activateResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var actBody = await activateResponse.Content.ReadAsStringAsync();
            var actDto = JsonConvert.DeserializeObject<VesselDto>(actBody);
            actDto.Active.Should().BeTrue();

            // 11) Hard delete
            var hardDeleteResponse = await client.DeleteAsync($"/api/Vessels/{created.Id}/hard");
            hardDeleteResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            // 12) Get by id should be NotFound
            /*var getAfterDelete = await client.GetAsync($"/api/Vessels/{created.Id}");
            getAfterDelete.StatusCode.Should().Be(HttpStatusCode.NotFound);*/

            // 13) Get by IMO should be NotFound
            var getByImoAfterDelete = await client.GetAsync($"/api/Vessels/imo/{created.ImoNumber}");
            getByImoAfterDelete.StatusCode.Should().Be(HttpStatusCode.NotFound);
        }

        private class TestApplicationFactory : WebApplicationFactory<DDDSample1.Program>
        {
            protected override void ConfigureWebHost(IWebHostBuilder builder)
            {
                builder.UseEnvironment("Testing");
                var projectRoot = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", ".."));
                builder.UseContentRoot(projectRoot);
            }
        }
    }
}
