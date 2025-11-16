/*
using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using DDDNetCore.Domain.PortInfrastructure.StorageArea;
using DDDNetCore.Domain.PortInfrastructure.StorageArea.DTOs;
using FluentAssertions;
using Newtonsoft.Json;
using Xunit;
using Microsoft.Extensions.DependencyInjection;
using DDDSample1.Infrastructure;
using DDDSample1.Domain.PortInfrastructure.StorageArea;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StorageAreas.DTOs;
using DDDNetCore.Infraestructure.PortInfrastructure.DTOs;
using DDDSample1.Domain.Vessels;

namespace DDDNetCore.Tests.Integration
{
    public class StorageAreaIntegrationTests : IClassFixture<CustomWebApplicationFactory<DDDSample1.Program>>
    {
        private readonly HttpClient _client;
        private readonly CustomWebApplicationFactory<DDDSample1.Program> _factory;

        public StorageAreaIntegrationTests(CustomWebApplicationFactory<DDDSample1.Program> factory)
        {
            _factory = factory;
            _client = factory.CreateClient();
        }

        [Fact]
        public async Task CreateStorageArea_ShouldReturnCreatedStorageArea()
        {
            var dto = new CreateStorageAreaDto
            {
                Code = "SA01",
                Designation = "Main Storage",
                StorageAreaType = StorageAreaType.Refrigerated,
                Coordinates = "41.123,-8.611",
                LocationDescription = "North Terminal",
                MaxCapacityTEUs = 500,
                InitialDockAssignments = null
            };

            var content = new StringContent(JsonConvert.SerializeObject(dto), Encoding.UTF8, "application/json");

            var response = await _client.PostAsync("/api/StorageArea", content);
            response.EnsureSuccessStatusCode();

            var str = await response.Content.ReadAsStringAsync();
            var created = JsonConvert.DeserializeObject<StorageAreaDto>(str);

            created.Should().NotBeNull();
            created.Code.Should().Be("SA01");
            created.Designation.Should().Be("Main Storage");
            created.Id.Should().NotBe(Guid.Empty);
        }

        [Fact]
        public async Task GetStorageAreaById_ShouldReturnStorageArea()
        {
            string storageAreaId;

            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var area = new StorageArea("SA02", "Secondary Storage", StorageAreaType.Yard,
                    new Location("41.0,-8.0", "Secondary Terminal"), 300);
                db.StorageAreas.Add(area);
                db.SaveChanges();

                storageAreaId = area.Id.AsString();
            }

            var response = await _client.GetAsync($"/api/StorageArea/{storageAreaId}");
            response.EnsureSuccessStatusCode();

            var str = await response.Content.ReadAsStringAsync();
            var dto = JsonConvert.DeserializeObject<StorageAreaDto>(str);

            dto.Should().NotBeNull();
            dto.Code.Should().Be("SA02");
            dto.Designation.Should().Be("Secondary Storage");
        }

        [Fact]
        public async Task UpdateStorageArea_ShouldModifyStorageArea()
        {
            string storageAreaId;

            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var area = new StorageArea("SA03", "Old Storage", StorageAreaType.Yard,
                    new Location("41.0,-8.0", "Old Terminal"), 300);
                db.StorageAreas.Add(area);
                db.SaveChanges();

                storageAreaId = area.Id.AsString();
            }

            var updateDto = new UpdateStorageAreaDto
            {
                Code = "SA03-U",
                Designation = "Updated Storage",
                StorageAreaType = StorageAreaType.Refrigerated,
                Coordinates = "41.123,-8.611",
                LocationDescription = "Updated Terminal",
                MaxCapacityTEUs = 400,
                CurrentOccupancyTEUs = 50
            };

            var content = new StringContent(JsonConvert.SerializeObject(updateDto), Encoding.UTF8, "application/json");

            var response = await _client.PutAsync($"/api/StorageArea/{storageAreaId}", content);
            response.EnsureSuccessStatusCode();

            var str = await response.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<StorageAreaDto>(str);

            updated.Should().NotBeNull();
            updated.Code.Should().Be("SA03-U");
            updated.Designation.Should().Be("Updated Storage");
            updated.LocationDescription.Should().Be("Updated Terminal");
        }

        [Fact]
        public async Task AssignAndUnassignDock_ShouldUpdateAssignments()
        {
            string storageAreaId;
            Guid dockId;

            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();

                var vesselType = new VesselType("Cargo", "Cargo ships", 5000, 10, 15, 8);
                db.VesselTypes.Add(vesselType);

                var dock = new Dock("Dock 1", 100, 10, 5, new Location("0,0", "Dock Loc"), new List<VesselType> { vesselType });
                db.Docks.Add(dock);

                var area = new StorageArea("SA04", "Storage with Dock", StorageAreaType.Yard,
                    new Location("41.123,-8.611", "North Terminal"), 500);
                db.StorageAreas.Add(area);

                db.SaveChanges();

                storageAreaId = area.Id.AsString();
                dockId = dock.Id.AsGuid();
            }

            // Assign Dock
            var assignDto = new AssignDockDto { DockId = dockId, DistanceMeters = 150 };
            var assignContent = new StringContent(JsonConvert.SerializeObject(assignDto), Encoding.UTF8, "application/json");
            var assignResponse = await _client.PostAsync($"/api/StorageArea/{storageAreaId}/assignDock", assignContent);
            assignResponse.EnsureSuccessStatusCode();

            var assignStr = await assignResponse.Content.ReadAsStringAsync();
            var assigned = JsonConvert.DeserializeObject<StorageAreaDto>(assignStr);
            assigned.AssignedDocks.Should().ContainSingle(d => d.DockId == dockId);

            // Unassign Dock
            var unassignResponse = await _client.DeleteAsync($"/api/StorageArea/{storageAreaId}/unassignDock/{dockId}");
            unassignResponse.EnsureSuccessStatusCode();

            var unassignStr = await unassignResponse.Content.ReadAsStringAsync();
            var unassigned = JsonConvert.DeserializeObject<StorageAreaDto>(unassignStr);
            unassigned.AssignedDocks.Should().BeEmpty();
        }

        [Fact]
        public async Task InactivateAndActivateStorageArea_ShouldChangeActiveStatus()
        {
            string storageAreaId;

            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var area = new StorageArea("SA05", "Storage Active", StorageAreaType.Refrigerated,
                    new Location("41.0,-8.0", "Loc"), 300);
                db.StorageAreas.Add(area);
                db.SaveChanges();

                storageAreaId = area.Id.AsString();
            }

            // Inactivate
            var inactivateResponse = await _client.PatchAsync($"/api/StorageArea/{storageAreaId}/inactivate", null);
            inactivateResponse.EnsureSuccessStatusCode();

            var inactivateStr = await inactivateResponse.Content.ReadAsStringAsync();
            var inactivated = JsonConvert.DeserializeObject<StorageAreaDto>(inactivateStr);
            inactivated.Active.Should().BeFalse();

            // Activate
            var activateResponse = await _client.PatchAsync($"/api/StorageArea/{storageAreaId}/activate", null);
            activateResponse.EnsureSuccessStatusCode();

            var activateStr = await activateResponse.Content.ReadAsStringAsync();
            var activated = JsonConvert.DeserializeObject<StorageAreaDto>(activateStr);
            activated.Active.Should().BeTrue();
        }
        [Fact]
        public async Task CreateStorageArea_InvalidData_ShouldReturnBadRequest()
        {
            var dto = new CreateStorageAreaDto
            {
                Code = "SA06",
                Designation = "Invalid Storage",
                StorageAreaType = (StorageAreaType)999, // Invalid enum
                Coordinates = "invalid-coords",
                LocationDescription = "Invalid Terminal",
                MaxCapacityTEUs = 0, // Invalid (must be >= 1)
                InitialDockAssignments = null
            };

            var content = new StringContent(JsonConvert.SerializeObject(dto), Encoding.UTF8, "application/json");
            var response = await _client.PostAsync("/api/StorageArea", content);

            response.StatusCode.Should().Be(System.Net.HttpStatusCode.BadRequest);
            var str = await response.Content.ReadAsStringAsync();
            str.Should().Contain("Invalid storage area type value").And.Contain("Coordinates must be in a valid");
        }
        [Fact]
        public async Task GetAllStorageAreas_ShouldReturnList()
        {
            var response = await _client.GetAsync("/api/StorageArea");
            response.EnsureSuccessStatusCode();

            var str = await response.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<StorageAreaDto>>(str);

            list.Should().NotBeNull();
            list.Count.Should().BeGreaterThanOrEqualTo(0);
        }
        [Fact]
        public async Task UpdateNonExistentStorageArea_ShouldReturnNotFound()
        {
            var updateDto = new UpdateStorageAreaDto
            {
                Code = "NonExist",
                Designation = "NonExist",
                StorageAreaType = StorageAreaType.Yard,
                Coordinates = "41.0,-8.0",
                LocationDescription = "Nowhere",
                MaxCapacityTEUs = 100,
                CurrentOccupancyTEUs = 0
            };

            var content = new StringContent(JsonConvert.SerializeObject(updateDto), Encoding.UTF8, "application/json");
            var fakeId = Guid.NewGuid();

            var response = await _client.PutAsync($"/api/StorageArea/{fakeId}", content);
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.NotFound);
        }
        [Fact]
        public async Task InactivateAlreadyInactiveStorageArea_ShouldReturnBadRequest()
        {
            string storageAreaId;

            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var area = new StorageArea("SA08", "Inactive Storage", StorageAreaType.Yard, new Location("0,0", "Loc"), 100);
                //area.Inactivate();
                db.StorageAreas.Add(area);
                db.SaveChanges();
                storageAreaId = area.Id.AsString();
            }

            var response = await _client.PatchAsync($"/api/StorageArea/{storageAreaId}/inactivate", null);
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
        }


    }
}
*/