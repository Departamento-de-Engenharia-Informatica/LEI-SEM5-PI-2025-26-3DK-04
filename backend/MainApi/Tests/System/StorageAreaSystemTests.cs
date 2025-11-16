/*
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using DDDNetCore.Domain.PortInfrastructure.StorageArea;
using DDDNetCore.Domain.PortInfrastructure.StorageArea.DTOs;
using FluentAssertions;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Newtonsoft.Json;
using Xunit;
using DDDSample1.Infrastructure;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.PortInfrastructure.StorageArea;
using DDDSample1.Domain.StorageAreas.DTOs;
using DDDNetCore.Infraestructure.PortInfrastructure.DTOs;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Vessels;

namespace DDDSample1.Tests.System
{
    public class StorageAreaSystemTests : IClassFixture<TestApplicationFactory>
    {
        private readonly TestApplicationFactory _factory;

        public StorageAreaSystemTests(TestApplicationFactory factory)
        {
            _factory = factory;
        }

        [Fact]
        public async Task StorageArea_Full_Workflow_via_HttpApi()
        {
            var client = _factory.CreateClient();

            // 1) Create VesselType for Dock
            Guid vesselTypeId;
            using (var scope = _factory.Services.GetRequiredService<IServiceScopeFactory>().CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Cargo", "Cargo ships", 5000, 10, 15, 8);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();
            }

            // 2) Create StorageArea
            var createDto = new CreateStorageAreaDto
            {
                Code = "SA100",
                Designation = "Main Storage",
                StorageAreaType = StorageAreaType.Refrigerated,
                Coordinates = "41.123,-8.611",
                LocationDescription = "North Terminal",
                MaxCapacityTEUs = 500
            };
            var createContent = new StringContent(JsonConvert.SerializeObject(createDto), Encoding.UTF8, "application/json");
            var createResponse = await client.PostAsync("/api/StorageArea", createContent);
            createResponse.StatusCode.Should().Be(HttpStatusCode.Created);
            var createdBody = await createResponse.Content.ReadAsStringAsync();
            var created = JsonConvert.DeserializeObject<StorageAreaDto>(createdBody);
            created.Should().NotBeNull();
            created.Id.Should().NotBe(Guid.Empty);

            // 3) Get by id
            var getResponse = await client.GetAsync($"/api/StorageArea/{created.Id}");
            getResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var getBody = await getResponse.Content.ReadAsStringAsync();
            var got = JsonConvert.DeserializeObject<StorageAreaDto>(getBody);
            got.Should().NotBeNull();
            got.Id.Should().Be(created.Id);

            // 4) Update StorageArea
            var updateDto = new UpdateStorageAreaDto
            {
                Code = "SA100-U",
                Designation = "Updated Storage",
                StorageAreaType = StorageAreaType.Yard,
                Coordinates = "41.125,-8.615",
                LocationDescription = "Updated Terminal",
                MaxCapacityTEUs = 600,
                CurrentOccupancyTEUs = 50
            };
            var updateContent = new StringContent(JsonConvert.SerializeObject(updateDto), Encoding.UTF8, "application/json");
            var updateResponse = await client.PutAsync($"/api/StorageArea/{created.Id}", updateContent);
            updateResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var updatedBody = await updateResponse.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<StorageAreaDto>(updatedBody);
            updated.Should().NotBeNull();
            updated.Code.Should().Be(updateDto.Code);
            updated.Designation.Should().Be(updateDto.Designation);

            // 5) Create Dock
            Guid dockId;
            using (var scope = _factory.Services.GetRequiredService<IServiceScopeFactory>().CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var dock = new Dock("Dock SA", 100, 10, 5, new Location("0,0", "Dock Loc"), new List<VesselType>
                {
                    db.VesselTypes.First(v => v.Id.AsGuid() == vesselTypeId)
                });
                db.Docks.Add(dock);
                db.SaveChanges();
                dockId = dock.Id.AsGuid();
            }

            // 6) Assign Dock
            var assignDto = new AssignDockDto { DockId = dockId, DistanceMeters = 200 };
            var assignContent = new StringContent(JsonConvert.SerializeObject(assignDto), Encoding.UTF8, "application/json");
            var assignResponse = await client.PostAsync($"/api/StorageArea/{created.Id}/assignDock", assignContent);
            assignResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var assignBody = await assignResponse.Content.ReadAsStringAsync();
            var assigned = JsonConvert.DeserializeObject<StorageAreaDto>(assignBody);
            assigned.AssignedDocks.Should().ContainSingle(d => d.DockId == dockId);

            // 7) Unassign Dock
            var unassignResponse = await client.DeleteAsync($"/api/StorageArea/{created.Id}/unassignDock/{dockId}");
            unassignResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var unassignBody = await unassignResponse.Content.ReadAsStringAsync();
            var unassigned = JsonConvert.DeserializeObject<StorageAreaDto>(unassignBody);
            unassigned.AssignedDocks.Should().BeEmpty();

            // 8) Inactivate StorageArea
            var inactivateResponse = await client.PatchAsync($"/api/StorageArea/{created.Id}/inactivate", null);
            inactivateResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var inactivateBody = await inactivateResponse.Content.ReadAsStringAsync();
            var inactivated = JsonConvert.DeserializeObject<StorageAreaDto>(inactivateBody);
            inactivated.Active.Should().BeFalse();

            // 9) Activate StorageArea
            var activateResponse = await client.PatchAsync($"/api/StorageArea/{created.Id}/activate", null);
            activateResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var activateBody = await activateResponse.Content.ReadAsStringAsync();
            var activated = JsonConvert.DeserializeObject<StorageAreaDto>(activateBody);
            activated.Active.Should().BeTrue();
        }
        
    }
}
*/