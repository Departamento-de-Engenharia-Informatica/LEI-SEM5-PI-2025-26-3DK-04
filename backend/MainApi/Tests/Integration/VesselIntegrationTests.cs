/*
using System;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using System.Collections.Generic;
using FluentAssertions;
using Newtonsoft.Json;
using Xunit;
using Microsoft.Extensions.DependencyInjection;
using DDDSample1.Infrastructure;
using DDDSample1.Domain.Vessels;

namespace DDDNetCore.Tests.Integration
{
    public class VesselIntegrationTests : IClassFixture<CustomWebApplicationFactory<DDDSample1.Program>>
    {
        private readonly HttpClient _client;
        private readonly CustomWebApplicationFactory<DDDSample1.Program> _factory;

        public VesselIntegrationTests(CustomWebApplicationFactory<DDDSample1.Program> factory)
        {
            _factory = factory;
            _client = factory.CreateClient();
        }

        [Fact]
        public async Task GetAll_ReturnsInsertedVessel()
        {
            Guid vesselTypeId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Container Ship", "Large cargo vessel", 1000, 10, 20, 8);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();

                var vessel = new Vessel("IMO1234567", "Test Vessel A", new VesselTypeId(vesselTypeId), "Owner A", "Operator A");
                db.Vessels.Add(vessel);
                db.SaveChanges();
            }

            var resp = await _client.GetAsync("/api/Vessels");
            resp.EnsureSuccessStatusCode();
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<VesselDto>>(str);
            list.Should().NotBeNull();
            list.Should().ContainSingle(v => v.ImoNumber == "IMO1234567" && v.Name == "Test Vessel A");
        }

        [Fact]
        public async Task GetById_ReturnsCorrectVessel()
        {
            Guid vesselTypeId;
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Bulk Carrier", "Bulk cargo vessel", 1500, 12, 22, 9);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();

                var vessel = new Vessel("IMO2345678", "Test Vessel B", new VesselTypeId(vesselTypeId), "Owner B", "Operator B");
                db.Vessels.Add(vessel);
                db.SaveChanges();
                insertedId = vessel.Id.AsGuid();
            }

            var resp = await _client.GetAsync($"/api/Vessels/{insertedId}");
            resp.EnsureSuccessStatusCode();
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var str = await resp.Content.ReadAsStringAsync();
            var dto = JsonConvert.DeserializeObject<VesselDto>(str);
            dto.Should().NotBeNull();
            dto.Id.Should().Be(insertedId);
            dto.ImoNumber.Should().Be("IMO2345678");
            dto.Name.Should().Be("Test Vessel B");
        }

        [Fact]
        public async Task GetByImoNumber_ReturnsCorrectVessel()
        {
            Guid vesselTypeId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Tanker", "Oil tanker", 2000, 15, 25, 10);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();

                var vessel = new Vessel("IMO3456789", "Test Vessel C", new VesselTypeId(vesselTypeId), "Owner C", "Operator C");
                db.Vessels.Add(vessel);
                db.SaveChanges();
            }

            var resp = await _client.GetAsync("/api/Vessels/imo/IMO3456789");
            resp.EnsureSuccessStatusCode();
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var str = await resp.Content.ReadAsStringAsync();
            var dto = JsonConvert.DeserializeObject<VesselDto>(str);
            dto.Should().NotBeNull();
            dto.ImoNumber.Should().Be("IMO3456789");
            dto.Name.Should().Be("Test Vessel C");
        }

        [Fact]
        public async Task Create_AddsVessel()
        {
            Guid vesselTypeId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Cargo Ship", "General cargo", 1200, 11, 21, 8);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();
            }

            var createDto = new CreatingVesselDto 
            { 
                ImoNumber = "IMO4567890", 
                Name = "Test Vessel D", 
                VesselTypeId = vesselTypeId,
                Owner = "Owner D",
                Operator = "Operator D"
            };
            var jsonContent = JsonConvert.SerializeObject(createDto);
            var content = new StringContent(jsonContent, Encoding.UTF8, "application/json");

            var response = await _client.PostAsync("/api/Vessels", content);

            response.StatusCode.Should().Be(System.Net.HttpStatusCode.Created);
            var responseBody = await response.Content.ReadAsStringAsync();
            responseBody.Should().NotBeNullOrWhiteSpace();
            var created = JsonConvert.DeserializeObject<VesselDto>(responseBody);
            created.Should().NotBeNull();
            created.ImoNumber.Should().Be("IMO4567890");
            created.Name.Should().Be("Test Vessel D");
            created.Owner.Should().Be("Owner D");
            created.Operator.Should().Be("Operator D");
            created.Id.Should().NotBe(Guid.Empty);
        }

        [Fact]
        public async Task Update_ChangesVessel()
        {
            Guid vesselTypeId;
            Guid newVesselTypeId;
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType1 = new VesselType("Type 1", "Desc 1", 1000, 10, 20, 8);
                var vesselType2 = new VesselType("Type 2", "Desc 2", 1100, 11, 21, 9);
                db.VesselTypes.Add(vesselType1);
                db.VesselTypes.Add(vesselType2);
                db.SaveChanges();
                vesselTypeId = vesselType1.Id.AsGuid();
                newVesselTypeId = vesselType2.Id.AsGuid();

                var vessel = new Vessel("IMO5678901", "Test Vessel E", new VesselTypeId(vesselTypeId), "Owner E", "Operator E");
                db.Vessels.Add(vessel);
                db.SaveChanges();
                insertedId = vessel.Id.AsGuid();
            }

            var updateDto = new UpdatingVesselDto 
            { 
                Name = "Updated Vessel E", 
                VesselTypeId = newVesselTypeId,
                Owner = "Updated Owner E",
                Operator = "Updated Operator E"
            };
            var jsonContent = JsonConvert.SerializeObject(updateDto);
            var content = new StringContent(jsonContent, Encoding.UTF8, "application/json");

            var response = await _client.PutAsync($"/api/Vessels/{insertedId}", content);
            response.EnsureSuccessStatusCode();
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var responseBody = await response.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<VesselDto>(responseBody);
            updated.Should().NotBeNull();
            updated.Id.Should().Be(insertedId);
            updated.Name.Should().Be("Updated Vessel E");
            updated.VesselTypeId.Should().Be(newVesselTypeId);
            updated.Owner.Should().Be("Updated Owner E");
            updated.Operator.Should().Be("Updated Operator E");
        }

        [Fact]
        public async Task SoftDelete_InactivatesVessel()
        {
            Guid vesselTypeId;
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Type F", "Desc F", 1000, 10, 20, 8);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();

                var vessel = new Vessel("IMO6789012", "Test Vessel F", new VesselTypeId(vesselTypeId), "Owner F", "Operator F");
                db.Vessels.Add(vessel);
                db.SaveChanges();
                insertedId = vessel.Id.AsGuid();
            }

            var response = await _client.DeleteAsync($"/api/Vessels/{insertedId}");
            response.EnsureSuccessStatusCode();
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var body = await response.Content.ReadAsStringAsync();
            var dto = JsonConvert.DeserializeObject<VesselDto>(body);
            dto.Active.Should().BeFalse();
        }

        [Fact]
        public async Task Activate_ActivatesVessel()
        {
            Guid vesselTypeId;
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Type G", "Desc G", 1000, 10, 20, 8);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();

                var vessel = new Vessel("IMO7890123", "Test Vessel G", new VesselTypeId(vesselTypeId), "Owner G", "Operator G");
                vessel.MarkAsInactive();
                db.Vessels.Add(vessel);
                db.SaveChanges();
                insertedId = vessel.Id.AsGuid();
            }

            var response = await _client.PostAsync($"/api/Vessels/{insertedId}/activate", null);
            response.EnsureSuccessStatusCode();
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var body = await response.Content.ReadAsStringAsync();
            var dto = JsonConvert.DeserializeObject<VesselDto>(body);
            dto.Active.Should().BeTrue();
        }

        [Fact]
        public async Task HardDelete_RemovesVessel()
        {
            Guid vesselTypeId;
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Type H", "Desc H", 1000, 10, 20, 8);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();

                var vessel = new Vessel("IMO8901234", "Test Vessel H", new VesselTypeId(vesselTypeId), "Owner H", "Operator H");
                db.Vessels.Add(vessel);
                db.SaveChanges();
                insertedId = vessel.Id.AsGuid();
            }

            var response = await _client.DeleteAsync($"/api/Vessels/{insertedId}/hard");
            response.EnsureSuccessStatusCode();
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);

            var getResponse = await _client.GetAsync($"/api/Vessels/{insertedId}");
            getResponse.StatusCode.Should().Be(System.Net.HttpStatusCode.NotFound);
        }

        [Fact]
        public async Task SearchByName_ReturnsMatches()
        {
            Guid vesselTypeId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Type Search", "Desc", 1000, 10, 20, 8);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();

                db.Vessels.Add(new Vessel("IMO1111111", "Alpha Carrier", new VesselTypeId(vesselTypeId), "Owner", "Operator"));
                db.Vessels.Add(new Vessel("IMO2222222", "Beta Carrier", new VesselTypeId(vesselTypeId), "Owner", "Operator"));
                db.Vessels.Add(new Vessel("IMO3333333", "Gamma Ship", new VesselTypeId(vesselTypeId), "Owner", "Operator"));
                db.SaveChanges();
            }

            var resp = await _client.GetAsync("/api/Vessels/search/name?term=Carrier");
            resp.EnsureSuccessStatusCode();
            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<VesselDto>>(str);
            list.Should().NotBeNull();
            list.Should().OnlyContain(v => v.Name.Contains("Carrier"));
        }

        [Fact]
        public async Task SearchByOwner_ReturnsMatches()
        {
            Guid vesselTypeId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Type Owner Search", "Desc", 1000, 10, 20, 8);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();

                db.Vessels.Add(new Vessel("IMO4444444", "Vessel Alpha", new VesselTypeId(vesselTypeId), "Maersk Corp", "Operator"));
                db.Vessels.Add(new Vessel("IMO5555555", "Vessel Beta", new VesselTypeId(vesselTypeId), "Maersk Line", "Operator"));
                db.Vessels.Add(new Vessel("IMO6666666", "Vessel Gamma", new VesselTypeId(vesselTypeId), "MSC Corp", "Operator"));
                db.SaveChanges();
            }

            var resp = await _client.GetAsync("/api/Vessels/search/owner?term=Maersk");
            resp.EnsureSuccessStatusCode();
            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<VesselDto>>(str);
            list.Should().NotBeNull();
            list.Should().OnlyContain(v => v.Owner.Contains("Maersk"));
        }

        [Fact]
        public async Task SearchByOperator_ReturnsMatches()
        {
            Guid vesselTypeId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Type Operator Search", "Desc", 1000, 10, 20, 8);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();

                db.Vessels.Add(new Vessel("IMO7777777", "Vessel Delta", new VesselTypeId(vesselTypeId), "Owner", "Evergreen Marine"));
                db.Vessels.Add(new Vessel("IMO8888888", "Vessel Epsilon", new VesselTypeId(vesselTypeId), "Owner", "Evergreen Shipping"));
                db.Vessels.Add(new Vessel("IMO9999999", "Vessel Zeta", new VesselTypeId(vesselTypeId), "Owner", "CMA CGM"));
                db.SaveChanges();
            }

            var resp = await _client.GetAsync("/api/Vessels/search/operator?term=Evergreen");
            resp.EnsureSuccessStatusCode();
            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<VesselDto>>(str);
            list.Should().NotBeNull();
            list.Should().OnlyContain(v => v.Operator.Contains("Evergreen"));
        }

        [Fact]
        public async Task SearchQueryParam_ReturnsMatches()
        {
            Guid vesselTypeId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var vesselType = new VesselType("Type Query Search", "Desc", 1000, 10, 20, 8);
                db.VesselTypes.Add(vesselType);
                db.SaveChanges();
                vesselTypeId = vesselType.Id.AsGuid();

                db.Vessels.Add(new Vessel("IMO0000001", "Pacific Explorer", new VesselTypeId(vesselTypeId), "Owner", "Operator"));
                db.Vessels.Add(new Vessel("IMO0000002", "Atlantic Voyager", new VesselTypeId(vesselTypeId), "Owner", "Operator"));
                db.Vessels.Add(new Vessel("IMO0000003", "Indian Explorer", new VesselTypeId(vesselTypeId), "Owner", "Operator"));
                db.SaveChanges();
            }

            var resp = await _client.GetAsync("/api/Vessels?search=Explorer");
            resp.EnsureSuccessStatusCode();
            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<VesselDto>>(str);
            list.Should().NotBeNull();
            list.Should().OnlyContain(v => v.Name.Contains("Explorer"));
        }
    }
}
*/