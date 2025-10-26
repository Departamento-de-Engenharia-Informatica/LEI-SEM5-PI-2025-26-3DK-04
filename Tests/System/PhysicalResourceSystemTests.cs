using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using DDDSample1.Domain.PhysicalResources;
using DDDSample1.Domain.PhysicalResources.DTOs;
using DDDSample1.Domain.Qualifications;
using DDDSample1.Domain.Shared;
using FluentAssertions;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Newtonsoft.Json;
using Xunit;
using DDDSample1.Infrastructure;

namespace DDDSample1.Tests.System
{
    public class PhysicalResourceSystemTests : IClassFixture<WebApplicationFactory<DDDSample1.Program>>
    {
        private readonly WebApplicationFactory<DDDSample1.Program> _factory;

        public PhysicalResourceSystemTests()
        {
            _factory = new TestApplicationFactory();
        }

        [Fact]
        public async Task PhysicalResource_Full_Workflow_via_HttpApi()
        {
            var client = _factory.CreateClient();

            // 1) Create Qualification
            Guid qualId;
            using (var scope = _factory.Services.GetRequiredService<IServiceScopeFactory>().CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var qual = new Qualification("Forklift License");
                db.Qualifications.Add(qual);
                db.SaveChanges();
                qualId = qual.Id.AsGuid();
            }

            // 2) Create PhysicalResource
            var createDto = new CreatePhysicalResourceDto
            {
                Description = "Forklift",
                Type = "Vehicle",
                Capacity = 2000,
                AssignedArea = "Warehouse A",
                SetupTime = 15,
                Status = ResourceStatus.Active,
                QualificationIds = new List<QualificationID> { new QualificationID(qualId) }
            };
            var createContent = new StringContent(JsonConvert.SerializeObject(createDto), Encoding.UTF8, "application/json");
            var createResponse = await client.PostAsync("/api/PhysicalResources", createContent);
            createResponse.StatusCode.Should().Be(HttpStatusCode.Created);

            var createdBody = await createResponse.Content.ReadAsStringAsync();
            var created = JsonConvert.DeserializeObject<PhysicalResourceDto>(createdBody);
            created.Should().NotBeNull();
            created.Id.Should().NotBe(Guid.Empty);
            created.QualificationIds.Should().Contain(qualId.ToString());

            // 3) Get by Id
            var getResponse = await client.GetAsync($"/api/PhysicalResources/{created.Id}");
            getResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var getBody = await getResponse.Content.ReadAsStringAsync();
            var got = JsonConvert.DeserializeObject<PhysicalResourceDto>(getBody);
            got.Should().NotBeNull();
            got.Id.Should().Be(created.Id);

            // 4) Update PhysicalResource
            var updateDto = new UpdatePhysicalResourceDto
            {
                Description = "Electric Forklift",
                Capacity = 2500,
                AssignedArea = "Warehouse B",
                SetupTime = 20,
                QualificationIds = new List<QualificationID>() // empty for simplicity
            };
            var updateContent = new StringContent(JsonConvert.SerializeObject(updateDto), Encoding.UTF8, "application/json");
            var updateResponse = await client.PutAsync($"/api/PhysicalResources/{created.Id}", updateContent);
            updateResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var updatedBody = await updateResponse.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<PhysicalResourceDto>(updatedBody);
            updated.Description.Should().Be(updateDto.Description);
            updated.Capacity.Should().Be(updateDto.Capacity);
            updated.AssignedArea.Should().Be(updateDto.AssignedArea);

            // 5) Change Status to Inactive
            var changeStatusDto = new ChangeStatusDto { NewStatus = ResourceStatus.Inactive };
            var statusContent = new StringContent(JsonConvert.SerializeObject(changeStatusDto), Encoding.UTF8, "application/json");
            var statusResponse = await client.PatchAsync($"/api/PhysicalResources/{created.Id}/status", statusContent);
            statusResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var statusBody = await statusResponse.Content.ReadAsStringAsync();
            var statusChanged = JsonConvert.DeserializeObject<PhysicalResourceDto>(statusBody);
            statusChanged.Status.Should().Be(ResourceStatus.Inactive);

            // 6) Change Status back to Active
            changeStatusDto.NewStatus = ResourceStatus.Active;
            statusContent = new StringContent(JsonConvert.SerializeObject(changeStatusDto), Encoding.UTF8, "application/json");
            statusResponse = await client.PatchAsync($"/api/PhysicalResources/{created.Id}/status", statusContent);
            statusResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            statusBody = await statusResponse.Content.ReadAsStringAsync();
            statusChanged = JsonConvert.DeserializeObject<PhysicalResourceDto>(statusBody);
            statusChanged.Status.Should().Be(ResourceStatus.Active);

            // 7) Search PhysicalResources
            var searchResponse = await client.GetAsync("/api/PhysicalResources?type=Vehicle");
            searchResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var searchBody = await searchResponse.Content.ReadAsStringAsync();
            var results = JsonConvert.DeserializeObject<List<PhysicalResourceDto>>(searchBody);
            results.Should().ContainSingle(r => r.Id == created.Id);
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
