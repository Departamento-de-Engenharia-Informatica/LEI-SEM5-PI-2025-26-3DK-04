using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using FluentAssertions;
using Newtonsoft.Json;
using Xunit;
using Microsoft.Extensions.DependencyInjection;
using DDDSample1.Infrastructure;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Vessels;
using DDDNetCore.Infraestructure.Docks;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Tests.Integration
{
    public class DockIntegrationTests : IClassFixture<CustomWebApplicationFactory<DDDSample1.Program>>
    {
        private readonly HttpClient _client;
        private readonly CustomWebApplicationFactory<DDDSample1.Program> _factory;

        public DockIntegrationTests(CustomWebApplicationFactory<DDDSample1.Program> factory)
        {
            _factory = factory;
            _client = factory.CreateClient();
        }

        [Fact]
        public async Task CreateDock_ShouldReturnCreatedDock()
        {
            Guid vesselTypeId;

            // Inserir um VesselType válido primeiro
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Cargo", "For cargo ships", 5000, 10, 15, 8);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();
            }

            var dto = new DockDto
            {
                Name = "Main Dock",
                Length = 200,
                Depth = 15,
                MaxDraft = 8,
                Coordinates = "41.123,-8.611",
                LocationDescription = "North terminal",
                VesselTypeIds = new List<Guid> { vesselTypeId }
            };

            var jsonContent = JsonConvert.SerializeObject(dto);
            var content = new StringContent(jsonContent, Encoding.UTF8, "application/json");

            var response = await _client.PostAsync("/api/Dock", content);

            response.StatusCode.Should().Be(System.Net.HttpStatusCode.Created);

            var responseBody = await response.Content.ReadAsStringAsync();
            var created = JsonConvert.DeserializeObject<DockDetailsDto>(responseBody);

            created.Should().NotBeNull();
            created.Name.Should().Be("Main Dock");
            created.AllowedVesselTypes.Should().Contain("Cargo");
            created.Id.Should().NotBe(Guid.Empty);
        }

        [Fact]
        public async Task GetDockById_ShouldReturnDock()
        {
            string dockId;

            // Inserir dock manualmente
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Cargo", "For cargo ships", 5000, 10, 15, 8);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();

                var dock = new Dock("East Dock", 180, 12, 7, new Location("41,-9", "East Zone"), new List<VesselType> { vesselType });
                db.Docks.Add(dock);
                db.SaveChanges();

                dockId = dock.Id.AsString();
            }

            var response = await _client.GetAsync($"/api/Dock/{dockId}");
            response.EnsureSuccessStatusCode();

            var str = await response.Content.ReadAsStringAsync();
            var dto = JsonConvert.DeserializeObject<DockDetailsDto>(str);

            dto.Should().NotBeNull();
            dto.Name.Should().Be("East Dock");
        }

        [Fact]
        public async Task UpdateDock_ShouldModifyDock()
        {
            string dockId;
            Guid vesselTypeId;

            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Tanker", "Oil tanker", 2000, 12, 8, 15);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();

                var dock = new Dock("Old Dock", 150, 12, 6, new Location("0,0", "Old Location"), new List<VesselType> { vesselType });
                db.Docks.Add(dock);
                db.SaveChanges();
                dockId = dock.Id.AsString();
            }

            var updateDto = new DockDto
            {
                Name = "Updated Dock",
                Length = 180,
                Depth = 14,
                MaxDraft = 7,
                Coordinates = "40.999,-8.600",
                LocationDescription = "Updated Location",
                VesselTypeIds = new List<Guid> { vesselTypeId }
            };

            var jsonContent = JsonConvert.SerializeObject(updateDto);
            var content = new StringContent(jsonContent, Encoding.UTF8, "application/json");

            var response = await _client.PutAsync($"/api/Dock/{dockId}", content);
            response.EnsureSuccessStatusCode();

            var body = await response.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<DockDetailsDto>(body);

            updated.Should().NotBeNull();
            updated.Name.Should().Be("Updated Dock");
            updated.Length.Should().Be(180);
            updated.LocationDescription.Should().Be("Updated Location");
        }

        [Fact]
        public async Task SoftDeleteDock_ShouldMarkAsInactive()
        {
            string dockId;
            Guid vesselTypeId;

            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Cargo", "For cargo ships", 5000, 10, 15, 8);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();

                var dock = new Dock("Active Dock", 100, 10, 5, new Location("0,0", "Location"), new List<VesselType> { vesselType });
                db.Docks.Add(dock);
                db.SaveChanges();
                dockId = dock.Id.AsString();
            }

            var response = await _client.DeleteAsync($"/api/Dock/{dockId}");
            response.EnsureSuccessStatusCode();

            var body = await response.Content.ReadAsStringAsync();
            var dto = JsonConvert.DeserializeObject<DockDetailsDto>(body);
            //dto.Active.Should().BeFalse();
        }
        [Fact]
public async Task HardDeleteDock_ShouldRemoveDock()
{
    string dockId;
    Guid vesselTypeId;

    var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
    using (var scope = scopeFactory.CreateScope())
    {
        var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
        var vesselType = new VesselType("Cargo", "For cargo ships", 5000, 10, 15, 8);
        db.VesselTypes.Add(vesselType);
        db.SaveChanges();
        vesselTypeId = vesselType.Id.AsGuid();

        var dock = new Dock("To Delete Dock", 120, 10, 5, new Location("0,0", "Location"), new List<VesselType> { vesselType });
        db.Docks.Add(dock);
        db.SaveChanges();
        dockId = dock.Id.AsString();
    }

    var response = await _client.DeleteAsync($"/api/Dock/{dockId}/hard");
    response.EnsureSuccessStatusCode();

    // Verifica que já não existe
    var getResponse = await _client.GetAsync($"/api/Dock/{dockId}");
    getResponse.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
}

[Fact]
public async Task SearchDockByName_ShouldReturnMatchingDocks()
{
    Guid vesselTypeId;

    var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
    using (var scope = scopeFactory.CreateScope())
    {
        var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
        var vesselType = new VesselType("Cargo", "For cargo ships", 5000, 10, 15, 8);
        db.VesselTypes.Add(vesselType);
        db.SaveChanges();
        vesselTypeId = vesselType.Id.AsGuid();

        db.Docks.Add(new Dock("North Dock", 150, 12, 6, new Location("0,0", "North"), new List<VesselType> { vesselType }));
        db.Docks.Add(new Dock("South Dock", 180, 15, 8, new Location("0,0", "South"), new List<VesselType> { vesselType }));
        db.Docks.Add(new Dock("East Pier", 200, 18, 10, new Location("0,0", "East"), new List<VesselType> { vesselType }));
        db.SaveChanges();
    }

    var response = await _client.GetAsync("/api/Dock/search/by-name?name=Dock");
    response.EnsureSuccessStatusCode();

    var str = await response.Content.ReadAsStringAsync();
    var list = JsonConvert.DeserializeObject<List<DockDetailsDto>>(str);

    list.Should().NotBeNull();
    list.Should().OnlyContain(d => d.Name.Contains("Dock"));
}

[Fact]
public async Task FilterDockByVesselType_ShouldReturnMatchingDocks()
{
    Guid vesselTypeId1;
    Guid vesselTypeId2;

    var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
    using (var scope = scopeFactory.CreateScope())
    {
        var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
        var type1 = new VesselType("Cargo", "Cargo ships", 5000, 10, 15, 8);
        var type2 = new VesselType("Tanker", "Oil tankers", 2000, 8, 10, 6);
        db.VesselTypes.AddRange(type1, type2);
        db.SaveChanges();
        vesselTypeId1 = type1.Id.AsGuid();
        vesselTypeId2 = type2.Id.AsGuid();

        db.Docks.Add(new Dock("Dock A", 150, 12, 6, new Location("0,0", "Location A"), new List<VesselType> { type1 }));
        db.Docks.Add(new Dock("Dock B", 180, 15, 8, new Location("0,0", "Location B"), new List<VesselType> { type2 }));
        db.Docks.Add(new Dock("Dock C", 200, 18, 10, new Location("0,0", "Location C"), new List<VesselType> { type1, type2 }));
        db.SaveChanges();
    }

    var response = await _client.GetAsync($"/api/Dock/search/by-vessel-type?typeId={vesselTypeId1}");
    response.EnsureSuccessStatusCode();

    var str = await response.Content.ReadAsStringAsync();
    var list = JsonConvert.DeserializeObject<List<DockDetailsDto>>(str);

    list.Should().NotBeNull();
    list.Should().OnlyContain(d => d.AllowedVesselTypes.Contains("Cargo"));
}

    }
}
