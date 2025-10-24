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
    public class VesselTypeIntegrationTests : IClassFixture<CustomWebApplicationFactory<DDDSample1.Program>>
    {
        private readonly HttpClient _client;
        private readonly CustomWebApplicationFactory<DDDSample1.Program> _factory;

        public VesselTypeIntegrationTests(CustomWebApplicationFactory<DDDSample1.Program> factory)
        {
            _factory = factory;
            _client = factory.CreateClient();
        }

        [Fact]
        public async Task GetAll_ReturnsInsertedVesselType()
        {
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var entity = new VesselType("Type A", "Desc A", 100, 2, 3, 4);
                db.VesselTypes.Add(entity);
                db.SaveChanges();
            }

            var resp = await _client.GetAsync("/api/VesselTypes");
            resp.EnsureSuccessStatusCode();
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<VesselTypeDto>>(str);
            list.Should().NotBeNull();
            list.Should().ContainSingle(v => v.Name == "Type A" && v.Description == "Desc A");
        }

        [Fact]
        public async Task GetById_ReturnsCorrectVesselType()
        {
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var entity = new VesselType("Type B", "Desc B", 200, 3, 4, 5);
                db.VesselTypes.Add(entity);
                db.SaveChanges();
                insertedId = entity.Id.AsGuid();
            }

            var resp = await _client.GetAsync($"/api/VesselTypes/{insertedId}");
            resp.EnsureSuccessStatusCode();
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var str = await resp.Content.ReadAsStringAsync();
            var dto = JsonConvert.DeserializeObject<VesselTypeDto>(str);
            dto.Should().NotBeNull();
            dto.Id.Should().Be(insertedId);
            dto.Name.Should().Be("Type B");
        }

        [Fact]
        public async Task Create_AddsVesselType()
        {
            var createDto = new CreatingVesselTypeDto { Name = "Type C", Description = "Desc C", Capacity = 300, MaxRows = 4, MaxBays = 5, MaxTiers = 6 };
            var jsonContent = JsonConvert.SerializeObject(createDto);
            var content = new StringContent(jsonContent, Encoding.UTF8, "application/json");

            var response = await _client.PostAsync("/api/VesselTypes", content);

            response.StatusCode.Should().Be(System.Net.HttpStatusCode.Created);
            var responseBody = await response.Content.ReadAsStringAsync();
            responseBody.Should().NotBeNullOrWhiteSpace();
            var created = JsonConvert.DeserializeObject<VesselTypeDto>(responseBody);
            created.Should().NotBeNull();
            created.Name.Should().Be("Type C");
            created.Description.Should().Be("Desc C");
            created.Capacity.Should().Be(300);
            created.Id.Should().NotBe(Guid.Empty);
        }

        [Fact]
        public async Task Update_ChangesVesselType()
        {
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var entity = new VesselType("Type D", "Desc D", 400, 5, 6, 7);
                db.VesselTypes.Add(entity);
                db.SaveChanges();
                insertedId = entity.Id.AsGuid();
            }

            var updateDto = new UpdatingVesselTypeDto { Name = "Type D2", Description = "Desc D2", Capacity = 450, MaxRows = 6, MaxBays = 7, MaxTiers = 8 };
            var jsonContent = JsonConvert.SerializeObject(updateDto);
            var content = new StringContent(jsonContent, Encoding.UTF8, "application/json");

            var response = await _client.PutAsync($"/api/VesselTypes/{insertedId}", content);
            response.EnsureSuccessStatusCode();
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var responseBody = await response.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<VesselTypeDto>(responseBody);
            updated.Should().NotBeNull();
            updated.Id.Should().Be(insertedId);
            updated.Name.Should().Be("Type D2");
            updated.Description.Should().Be("Desc D2");
            updated.Capacity.Should().Be(450);
        }

        [Fact]
        public async Task SoftDelete_InactivatesVesselType()
        {
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var entity = new VesselType("Type E", "Desc E", 500, 6, 7, 8);
                db.VesselTypes.Add(entity);
                db.SaveChanges();
                insertedId = entity.Id.AsGuid();
            }

            var response = await _client.DeleteAsync($"/api/VesselTypes/{insertedId}");
            response.EnsureSuccessStatusCode();
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var body = await response.Content.ReadAsStringAsync();
            var dto = JsonConvert.DeserializeObject<VesselTypeDto>(body);
            dto.Active.Should().BeFalse();
        }

        [Fact]
        public async Task Activate_ActivatesVesselType()
        {
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var entity = new VesselType("Type F", "Desc F", 600, 7, 8, 9);
                entity.MarkAsInactive();
                db.VesselTypes.Add(entity);
                db.SaveChanges();
                insertedId = entity.Id.AsGuid();
            }

            var response = await _client.PostAsync($"/api/VesselTypes/{insertedId}/activate", null);
            response.EnsureSuccessStatusCode();
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var body = await response.Content.ReadAsStringAsync();
            var dto = JsonConvert.DeserializeObject<VesselTypeDto>(body);
            dto.Active.Should().BeTrue();
        }

        [Fact]
        public async Task HardDelete_RemovesVesselType()
        {
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var entity = new VesselType("Type G", "Desc G", 700, 8, 9, 10);
                db.VesselTypes.Add(entity);
                db.SaveChanges();
                insertedId = entity.Id.AsGuid();
            }

            var response = await _client.DeleteAsync($"/api/VesselTypes/{insertedId}/hard");
            response.EnsureSuccessStatusCode();
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);

            var getResponse = await _client.GetAsync($"/api/VesselTypes/{insertedId}");
            getResponse.StatusCode.Should().Be(System.Net.HttpStatusCode.NotFound);
        }

        [Fact]
        public async Task SearchByName_ReturnsMatches()
        {
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                db.VesselTypes.Add(new VesselType("Alpha Carrier", "Alpha Desc", 100, 2, 3, 4));
                db.VesselTypes.Add(new VesselType("Beta Carrier", "Beta Desc", 120, 2, 3, 4));
                db.VesselTypes.Add(new VesselType("Gamma Ship", "Gamma Desc", 140, 2, 3, 4));
                db.SaveChanges();
            }

            var resp = await _client.GetAsync("/api/VesselTypes/search/name?term=Carrier");
            resp.EnsureSuccessStatusCode();
            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<VesselTypeDto>>(str);
            list.Should().NotBeNull();
            list.Should().OnlyContain(v => v.Name.Contains("Carrier"));
        }

        [Fact]
        public async Task SearchByDescription_ReturnsMatches()
        {
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                db.VesselTypes.Add(new VesselType("Alpha", "Heavy container", 100, 2, 3, 4));
                db.VesselTypes.Add(new VesselType("Beta", "Light container", 120, 2, 3, 4));
                db.VesselTypes.Add(new VesselType("Gamma", "Bulk carrier", 140, 2, 3, 4));
                db.SaveChanges();
            }

            var resp = await _client.GetAsync("/api/VesselTypes/search/description?term=container");
            resp.EnsureSuccessStatusCode();
            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<VesselTypeDto>>(str);
            list.Should().NotBeNull();
            list.Should().OnlyContain(v => v.Description.ToLower().Contains("container"));
        }

        [Fact]
        public async Task SearchQueryParam_ReturnsMatchesFromNameOrDescription()
        {
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                db.VesselTypes.Add(new VesselType("Omega Carrier", "Desc1", 100, 2, 3, 4));
                db.VesselTypes.Add(new VesselType("Zeta", "General carrier type", 120, 2, 3, 4));
                db.VesselTypes.Add(new VesselType("Sigma", "Misc", 140, 2, 3, 4));
                db.SaveChanges();
            }

            var resp = await _client.GetAsync("/api/VesselTypes?search=carrier");
            resp.EnsureSuccessStatusCode();
            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<VesselTypeDto>>(str);
            list.Should().NotBeNull();
            list.Should().OnlyContain(v => v.Name.ToLower().Contains("carrier") || v.Description.ToLower().Contains("carrier"));
        }
    }
}
