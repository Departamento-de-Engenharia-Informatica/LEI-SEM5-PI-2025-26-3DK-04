using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using DDDSample1.Domain.PhysicalResources;
using DDDSample1.Domain.PhysicalResources.DTOs;
using DDDSample1.Domain.Qualifications;
using DDDSample1.Domain.Shared;
using FluentAssertions;
using Newtonsoft.Json;
using Xunit;
using Microsoft.Extensions.DependencyInjection;
using DDDSample1.Infrastructure;

namespace DDDNetCore.Tests.Integration
{
    public class PhysicalResourceIntegrationTests : IClassFixture<CustomWebApplicationFactory<DDDSample1.Program>>
    {
        private readonly HttpClient _client;
        private readonly CustomWebApplicationFactory<DDDSample1.Program> _factory;

        public PhysicalResourceIntegrationTests(CustomWebApplicationFactory<DDDSample1.Program> factory)
        {
            _factory = factory;
            _client = factory.CreateClient();
        }

        // Helper to add a qualification and return its Guid
        private async Task<Guid> AddQualificationAsync(string name)
        {
            using var scope = _factory.Services.CreateScope();
            var db = scope.ServiceProvider.GetRequiredService<DDDSample1DbContext>();

            var qual = new Qualification(name);
            db.Qualifications.Add(qual);
            await db.SaveChangesAsync();
            return qual.Id.AsGuid();
        }

        // Helper to add a physical resource with a qualification
        private async Task<Guid> AddPhysicalResourceAsync(string description, string type, double capacity, string? assignedArea = null)
        {
            var qualId = await AddQualificationAsync($"{description} License");

            using var scope = _factory.Services.CreateScope();
            var db = scope.ServiceProvider.GetRequiredService<DDDSample1DbContext>();

            var qual = db.Qualifications.Single(q => q.Id.AsGuid() == qualId);

            var resource = new PhysicalResource(
                description,
                type,
                capacity,
                assignedArea,
                setupTime: 15,
                status: ResourceStatus.Active,
                qualifications: new List<Qualification> { qual }
            );

            db.PhysicalResources.Add(resource);
            await db.SaveChangesAsync();
            return resource.Id.AsGuid();
        }

        [Fact]
        public async Task CreatePhysicalResource_ShouldReturnCreatedResource()
        {
            var qualId = await AddQualificationAsync("Forklift License");

            var dto = new CreatePhysicalResourceDto
            {
                Description = "Forklift",
                Type = "Vehicle",
                Capacity = 2000,
                AssignedArea = "Warehouse A",
                SetupTime = 15,
                Status = ResourceStatus.Active,
                QualificationIds = new List<QualificationID> { new QualificationID(qualId) }
            };

            var content = new StringContent(JsonConvert.SerializeObject(dto), Encoding.UTF8, "application/json");
            var response = await _client.PostAsync("/api/PhysicalResources", content);
            response.EnsureSuccessStatusCode();

            var str = await response.Content.ReadAsStringAsync();
            var created = JsonConvert.DeserializeObject<PhysicalResourceDto>(str);

            created.Should().NotBeNull();
            created.Description.Should().Be("Forklift");
            created.Type.Should().Be("Vehicle");
            created.AssignedArea.Should().Be("Warehouse A");
            created.QualificationIds.Should().Contain(qualId.ToString());
        }

        [Fact]
        public async Task UpdatePhysicalResource_ShouldModifyResource()
        {
            var resourceId = await AddPhysicalResourceAsync("Forklift", "Vehicle", 2000, "Warehouse A");

            var updateDto = new UpdatePhysicalResourceDto
            {
                Description = "Electric Forklift",
                Capacity = 2500,
                AssignedArea = "Warehouse B",
                SetupTime = 20,
                QualificationIds = new List<QualificationID>() // Can add qualifications if needed
            };

            var content = new StringContent(JsonConvert.SerializeObject(updateDto), Encoding.UTF8, "application/json");
            var response = await _client.PutAsync($"/api/PhysicalResources/{resourceId}", content);
            response.EnsureSuccessStatusCode();

            var str = await response.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<PhysicalResourceDto>(str);

            updated.Should().NotBeNull();
            updated.Description.Should().Be("Electric Forklift");
            updated.Capacity.Should().Be(2500);
            updated.AssignedArea.Should().Be("Warehouse B");
            updated.SetupTime.Should().Be(20);
        }

        [Fact]
        public async Task ChangeStatus_ShouldUpdateStatus()
        {
            var resourceId = await AddPhysicalResourceAsync("Forklift", "Vehicle", 2000);

            var changeStatusDto = new ChangeStatusDto { NewStatus = ResourceStatus.Inactive };
            var content = new StringContent(JsonConvert.SerializeObject(changeStatusDto), Encoding.UTF8, "application/json");

            var response = await _client.PatchAsync($"/api/PhysicalResources/{resourceId}/status", content);
            response.EnsureSuccessStatusCode();

            var str = await response.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<PhysicalResourceDto>(str);

            updated.Status.Should().Be(ResourceStatus.Inactive);

            // Change back to active
            changeStatusDto.NewStatus = ResourceStatus.Active;
            content = new StringContent(JsonConvert.SerializeObject(changeStatusDto), Encoding.UTF8, "application/json");
            response = await _client.PatchAsync($"/api/PhysicalResources/{resourceId}/status", content);
            response.EnsureSuccessStatusCode();

            str = await response.Content.ReadAsStringAsync();
            updated = JsonConvert.DeserializeObject<PhysicalResourceDto>(str);
            updated.Status.Should().Be(ResourceStatus.Active);
        }

        [Fact]
        public async Task SearchPhysicalResources_ShouldFilterCorrectly()
        {
            var r1 = await AddPhysicalResourceAsync("Forklift", "Vehicle", 2000, "Warehouse A");
            var r2 = await AddPhysicalResourceAsync("Loader", "Vehicle", 3000, "Warehouse B");
            var r3 = await AddPhysicalResourceAsync("Crane", "Equipment", 5000);

            var response = await _client.GetAsync("/api/PhysicalResources?type=Vehicle");
            response.EnsureSuccessStatusCode();

            var str = await response.Content.ReadAsStringAsync();
            var results = JsonConvert.DeserializeObject<List<PhysicalResourceDto>>(str);

            results.Should().NotBeNull();
            //results.Count.Should().Be(2);
            results.Select(r => r.Description).Should().Contain(new[] { "Forklift", "Loader" });
        }
    }
}
