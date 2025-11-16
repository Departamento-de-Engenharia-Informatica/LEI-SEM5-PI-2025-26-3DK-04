/*
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

namespace DDDSample1.Tests.System
{
    public class VesselTypeSystemTests : IClassFixture<TestApplicationFactory>
    {
        private readonly TestApplicationFactory _factory;

        public VesselTypeSystemTests(TestApplicationFactory factory)
        {
            _factory = factory;
        }

        [Fact]
        public async Task VesselType_Full_Workflow_via_HttpApi()
        {
            var client = _factory.CreateClient();

            // 1) Create
            var createDto = new CreatingVesselTypeDto { Name = "System VT", Description = "System Desc", Capacity = 1000, MaxRows = 10, MaxBays = 20, MaxTiers = 8 };
            var createJson = JsonConvert.SerializeObject(createDto);
            var createContent = new StringContent(createJson, Encoding.UTF8, "application/json");

            var createResponse = await client.PostAsync("/api/VesselTypes", createContent);
            createResponse.StatusCode.Should().Be(HttpStatusCode.Created);

            var createdBody = await createResponse.Content.ReadAsStringAsync();
            var created = JsonConvert.DeserializeObject<VesselTypeDto>(createdBody);
            created.Should().NotBeNull();
            created.Id.Should().NotBe(Guid.Empty);
            created.Name.Should().Be(createDto.Name);
            created.Active.Should().BeTrue();

            // 2) Get by id
            var getResponse = await client.GetAsync($"/api/VesselTypes/{created.Id}");
            getResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var getBody = await getResponse.Content.ReadAsStringAsync();
            var got = JsonConvert.DeserializeObject<VesselTypeDto>(getBody);
            got.Should().NotBeNull();
            got.Id.Should().Be(created.Id);

            // 3) Update
            var updateDto = new UpdatingVesselTypeDto { Name = "System VT Updated", Description = "System Desc Updated", Capacity = 1100, MaxRows = 11, MaxBays = 22, MaxTiers = 9 };
            var updateJson = JsonConvert.SerializeObject(updateDto);
            var updateContent = new StringContent(updateJson, Encoding.UTF8, "application/json");
            var updateResponse = await client.PutAsync($"/api/VesselTypes/{created.Id}", updateContent);
            updateResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var updatedBody = await updateResponse.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<VesselTypeDto>(updatedBody);
            updated.Should().NotBeNull();
            updated.Name.Should().Be(updateDto.Name);
            updated.Description.Should().Be(updateDto.Description);
            updated.Capacity.Should().Be(updateDto.Capacity);

            // 4) Get all and ensure contains
            var allResponse = await client.GetAsync("/api/VesselTypes");
            allResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var allBody = await allResponse.Content.ReadAsStringAsync();
            var all = JsonConvert.DeserializeObject<VesselTypeDto[]>(allBody);
            all.Should().NotBeNull();
            all.Select(v => v.Id).Should().Contain(created.Id);

            // 5) Search by name
            var searchByName = await client.GetAsync("/api/VesselTypes/search/name?term=Updated");
            searchByName.StatusCode.Should().Be(HttpStatusCode.OK);

            // 6) Soft delete (inactivate)
            var softDeleteResponse = await client.DeleteAsync($"/api/VesselTypes/{created.Id}");
            softDeleteResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var softBody = await softDeleteResponse.Content.ReadAsStringAsync();
            var softDto = JsonConvert.DeserializeObject<VesselTypeDto>(softBody);
            softDto.Active.Should().BeFalse();

            // 7) Activate
            var activateResponse = await client.PostAsync($"/api/VesselTypes/{created.Id}/activate", null);
            activateResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var actBody = await activateResponse.Content.ReadAsStringAsync();
            var actDto = JsonConvert.DeserializeObject<VesselTypeDto>(actBody);
            actDto.Active.Should().BeTrue();

            // 8) Hard delete
            var hardDeleteResponse = await client.DeleteAsync($"/api/VesselTypes/{created.Id}/hard");
            hardDeleteResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            // 9) Get by id should be NotFound
            var getAfterDelete = await client.GetAsync($"/api/VesselTypes/{created.Id}");
            getAfterDelete.StatusCode.Should().Be(HttpStatusCode.NotFound);
        }
        
    }
}
*/