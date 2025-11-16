/*
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using FluentAssertions;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Newtonsoft.Json;
using Xunit;
using DDDSample1.Domain.Vessels;
using DDDSample1.Domain.Docks;
using DDDSample1.Infrastructure;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Authentication;
using Microsoft.Extensions.Logging;
using System.Text.Encodings.Web;
using System.Security.Claims;
using DDDNetCore.Infraestructure.Docks;
using Microsoft.Extensions.Options;

namespace DDDSample1.Tests.System
{
    public class DockSystemTests : IClassFixture<TestApplicationFactory>
    {
        private readonly TestApplicationFactory _factory;

        public DockSystemTests(TestApplicationFactory factory)
        {
            _factory = factory;
        }

        [Fact]
        public async Task Dock_Full_Workflow_via_HttpApi()
        {
            var client = _factory.CreateClient();

            // 1) Inserir um VesselType válido primeiro
            Guid vesselTypeId;
            using (var scope = _factory.Services.GetRequiredService<IServiceScopeFactory>().CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Cargo", "For cargo ships", 5000, 10, 15, 8);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();
            }

            // 2) Create Dock
            var createDto = new DockDto
            {
                Name = "Main Dock",
                Length = 200,
                Depth = 15,
                MaxDraft = 8,
                Coordinates = "41.123,-8.611",
                LocationDescription = "North terminal",
                VesselTypeIds = new List<Guid> { vesselTypeId }
            };
            var createJson = JsonConvert.SerializeObject(createDto);
            var createContent = new StringContent(createJson, Encoding.UTF8, "application/json");

            var createResponse = await client.PostAsync("/api/Dock", createContent);
            createResponse.StatusCode.Should().Be(HttpStatusCode.Created);

            var createdBody = await createResponse.Content.ReadAsStringAsync();
            var created = JsonConvert.DeserializeObject<DockDetailsDto>(createdBody);
            created.Should().NotBeNull();
            created.Id.Should().NotBe(Guid.Empty);
            created.Name.Should().Be(createDto.Name);
            created.AllowedVesselTypes.Should().Contain("Cargo");

            // 3) Get by id
            var getResponse = await client.GetAsync($"/api/Dock/{created.Id}");
            getResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var getBody = await getResponse.Content.ReadAsStringAsync();
            var got = JsonConvert.DeserializeObject<DockDetailsDto>(getBody);
            got.Should().NotBeNull();
            got.Id.Should().Be(created.Id);

            // 4) Update
            var updateDto = new DockDto
            {
                Name = "Main Dock Updated",
                Length = 220,
                Depth = 16,
                MaxDraft = 9,
                Coordinates = "41.125,-8.615",
                LocationDescription = "Updated terminal",
                VesselTypeIds = new List<Guid> { vesselTypeId }
            };
            var updateJson = JsonConvert.SerializeObject(updateDto);
            var updateContent = new StringContent(updateJson, Encoding.UTF8, "application/json");

            var updateResponse = await client.PutAsync($"/api/Dock/{created.Id}", updateContent);
            updateResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var updatedBody = await updateResponse.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<DockDetailsDto>(updatedBody);
            updated.Should().NotBeNull();
            updated.Name.Should().Be(updateDto.Name);
            updated.Length.Should().Be(220);
            updated.LocationDescription.Should().Be("Updated terminal");

            // 5) Search by name
            var searchResponse = await client.GetAsync("/api/Dock/search/by-name?name=Updated");
            searchResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var searchBody = await searchResponse.Content.ReadAsStringAsync();
            var searchResult = JsonConvert.DeserializeObject<DockDetailsDto[]>(searchBody);
            searchResult.Should().NotBeNull();
            searchResult.Select(d => d.Id).Should().Contain(created.Id);

            // 6) Filter by vessel type
            var filterResponse = await client.GetAsync($"/api/Dock/search/by-vessel-type?typeId={vesselTypeId}");
            filterResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var filterBody = await filterResponse.Content.ReadAsStringAsync();
            var filterResult = JsonConvert.DeserializeObject<DockDetailsDto[]>(filterBody);
            filterResult.Should().NotBeNull();
            filterResult.Select(d => d.Id).Should().Contain(created.Id);

            // 7) Soft delete
            var softDeleteResponse = await client.DeleteAsync($"/api/Dock/{created.Id}");
            softDeleteResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var softBody = await softDeleteResponse.Content.ReadAsStringAsync();
            var softDto = JsonConvert.DeserializeObject<DockDetailsDto>(softBody);
            softDto.Should().NotBeNull();

            // 8) Hard delete
            var hardDeleteResponse = await client.DeleteAsync($"/api/Dock/{created.Id}/hard");
            hardDeleteResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            // 9) Get by id should be OK but empty/NotFound
            var getAfterDelete = await client.GetAsync($"/api/Dock/{created.Id}");
            getAfterDelete.StatusCode.Should().Be(HttpStatusCode.OK);
        }
    }
}
*/