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
using DDDSample1.Domain.Qualifications;
using System.Text.Json;

namespace DDDNetCore.Tests.Integration
{
    public class QualificationIntegrationTests : IClassFixture<CustomWebApplicationFactory<DDDSample1.Program>>
    {
        private readonly HttpClient _client;
        private readonly CustomWebApplicationFactory<DDDSample1.Program> _factory;

        public QualificationIntegrationTests(CustomWebApplicationFactory<DDDSample1.Program> factory)
        {
            _factory = factory;
            _client = factory.CreateClient();
        }

        [Fact]
        public async Task GetAll_ReturnsInsertedQualification()
        {
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var entity = new Qualification("Test Qualification Name");
                db.Qualifications.Add(entity);
                db.SaveChanges();
            }

            var resp = await _client.GetAsync("/api/Qualifications");
            resp.EnsureSuccessStatusCode();
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<QualificationDto>>(str);
            list.Should().NotBeNull();
            list.Should().ContainSingle(q => q.Name == "Test Qualification Name");
        }

        [Fact]
        public async Task GetById_ReturnsCorrectQualification()
        {
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var entity = new Qualification("Specific Qualification Name");
                db.Qualifications.Add(entity);
                db.SaveChanges();
                insertedId = entity.Id.AsGuid();
            }

            var resp = await _client.GetAsync($"/api/Qualifications/{insertedId}");
            resp.EnsureSuccessStatusCode();
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var str = await resp.Content.ReadAsStringAsync();
            var dto = JsonConvert.DeserializeObject<QualificationDto>(str);
            dto.Should().NotBeNull();
            dto.Id.Should().Be(insertedId);
            dto.Name.Should().Be("Specific Qualification Name");
        }

        [Fact]
        public async Task Create_AddsQualification()
        {
            var createDto = new CreateQualificationDto { Name = "Created Qualification" };
            var jsonContent = JsonConvert.SerializeObject(createDto);
            var content = new StringContent(jsonContent, Encoding.UTF8, "application/json");

            var response = await _client.PostAsync("/api/Qualifications", content);

            response.StatusCode.Should().Be(System.Net.HttpStatusCode.Created);
            var responseBody = await response.Content.ReadAsStringAsync();
            responseBody.Should().NotBeNullOrWhiteSpace();
            var created = JsonConvert.DeserializeObject<QualificationDto>(responseBody);
            created.Should().NotBeNull();
            created.Name.Should().Be("Created Qualification");
            created.Id.Should().NotBe(Guid.Empty);
        }

        [Fact]
        public async Task Update_ChangesQualification()
        {
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var entity = new Qualification("Old Name");
                db.Qualifications.Add(entity);
                db.SaveChanges();
                insertedId = entity.Id.AsGuid();
            }

            var updateDto = new UpdateQualificationDto { Name = "New Name" };
            var jsonContent = JsonConvert.SerializeObject(updateDto);
            var content = new StringContent(jsonContent, Encoding.UTF8, "application/json");

            var response = await _client.PutAsync($"/api/Qualifications/{insertedId}", content);
            response.EnsureSuccessStatusCode();
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var responseBody = await response.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<QualificationDto>(responseBody);
            updated.Should().NotBeNull();
            updated.Id.Should().Be(insertedId);
            updated.Name.Should().Be("New Name");
        }

        [Fact]
        public async Task Delete_RemovesQualification()
        {
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var entity = new Qualification("To Be Deleted");
                db.Qualifications.Add(entity);
                db.SaveChanges();
                insertedId = entity.Id.AsGuid();
            }

            var response = await _client.DeleteAsync($"/api/Qualifications/{insertedId}");
            response.EnsureSuccessStatusCode();
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.NoContent);

            var getResponse = await _client.GetAsync($"/api/Qualifications/{insertedId}");
            getResponse.StatusCode.Should().Be(System.Net.HttpStatusCode.NotFound);
        }

        [Fact]
        public async Task SearchByName_ReturnsMatches()
        {
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                db.Qualifications.Add(new Qualification("Alpha Tester"));
                db.Qualifications.Add(new Qualification("Beta Tester"));
                db.Qualifications.Add(new Qualification("Unrelated Name"));
                db.SaveChanges();
            }

            var resp = await _client.GetAsync("/api/Qualifications/search?name=Tester");
            resp.EnsureSuccessStatusCode();
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<QualificationDto>>(str);
            list.Should().NotBeNull();
            list.Should().OnlyContain(q => q.Name.Contains("Tester"));
        }

        [Fact]
        public async Task ExistsById_ReturnsTrueOrFalse()
        {
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var entity = new Qualification("Exists Name");
                db.Qualifications.Add(entity);
                db.SaveChanges();
                insertedId = entity.Id.AsGuid();
            }

            var resp = await _client.GetAsync($"/api/Qualifications/exists/{insertedId}");
            resp.EnsureSuccessStatusCode();
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var str = await resp.Content.ReadAsStringAsync();
            // controller returns { Exists = true/false } but casing may vary depending on serializer settings
            var dict = JsonConvert.DeserializeObject<Dictionary<string, bool>>(str);
            bool exists = dict.ContainsKey("Exists") ? dict["Exists"] : dict.ContainsKey("exists") ? dict["exists"] : throw new KeyNotFoundException("Exists key not found in response");
            exists.Should().BeTrue();

            var random = Guid.NewGuid();
            var resp2 = await _client.GetAsync($"/api/Qualifications/exists/{random}");
            resp2.EnsureSuccessStatusCode();
            var str2 = await resp2.Content.ReadAsStringAsync();
            var dict2 = JsonConvert.DeserializeObject<Dictionary<string, bool>>(str2);
            bool exists2 = dict2.ContainsKey("Exists") ? dict2["Exists"] : dict2.ContainsKey("exists") ? dict2["exists"] : throw new KeyNotFoundException("Exists key not found in response");
            exists2.Should().BeFalse();
        }
    }
}