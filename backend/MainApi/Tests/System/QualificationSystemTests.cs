using System;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;
using FluentAssertions;
using Microsoft.AspNetCore.Hosting;
using System.IO;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Data.Sqlite;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using Newtonsoft.Json;
using Xunit;
using DDDSample1.Domain.Qualifications;
using DDDSample1.Infrastructure;

namespace DDDSample1.Tests.System
{
    public class QualificationSystemTests : IClassFixture<WebApplicationFactory<DDDSample1.Program>>
    {
        private readonly WebApplicationFactory<DDDSample1.Program> _factory;

        public QualificationSystemTests()
        {
            // Create a factory that forces the app environment to "Testing"
            _factory = new TestApplicationFactory();
        }

        [Fact]
        public async Task Qualification_CRUD_Workflow_via_HttpApi()
        {
            var client = _factory.CreateClient();

            // 1) Create
            var createDto = new CreateQualificationDto { Name = "System Test Qualification" };
            var createJson = JsonConvert.SerializeObject(createDto);
            var createContent = new StringContent(createJson, Encoding.UTF8, "application/json");

            var createResponse = await client.PostAsync("/api/Qualifications", createContent);
            createResponse.StatusCode.Should().Be(HttpStatusCode.Created);

            var createdBody = await createResponse.Content.ReadAsStringAsync();
            var created = JsonConvert.DeserializeObject<QualificationDto>(createdBody);
            created.Should().NotBeNull();
            created.Id.Should().NotBe(Guid.Empty);
            created.Name.Should().Be(createDto.Name);

            // 2) Get by id
            var getResponse = await client.GetAsync($"/api/Qualifications/{created.Id}");
            getResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var getBody = await getResponse.Content.ReadAsStringAsync();
            var got = JsonConvert.DeserializeObject<QualificationDto>(getBody);
            got.Should().NotBeNull();
            got.Id.Should().Be(created.Id);

            // 3) Update
            var updateDto = new UpdateQualificationDto { Name = "System Test Qualification Updated" };
            var updateJson = JsonConvert.SerializeObject(updateDto);
            var updateContent = new StringContent(updateJson, Encoding.UTF8, "application/json");
            var updateResponse = await client.PutAsync($"/api/Qualifications/{created.Id}", updateContent);
            updateResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var updatedBody = await updateResponse.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<QualificationDto>(updatedBody);
            updated.Should().NotBeNull();
            updated.Name.Should().Be(updateDto.Name);

            // 4) Get all and ensure contains
            var allResponse = await client.GetAsync("/api/Qualifications");
            allResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var allBody = await allResponse.Content.ReadAsStringAsync();
            var all = JsonConvert.DeserializeObject<QualificationDto[]>(allBody);
            all.Should().NotBeNull();
            all.Select(q => q.Id).Should().Contain(created.Id);

            // 5) Delete
            var deleteResponse = await client.DeleteAsync($"/api/Qualifications/{created.Id}");
            deleteResponse.StatusCode.Should().Be(HttpStatusCode.NoContent);

            // 6) Get by id should be NotFound
            var getAfterDelete = await client.GetAsync($"/api/Qualifications/{created.Id}");
            getAfterDelete.StatusCode.Should().Be(HttpStatusCode.NotFound);
        }

        private class TestApplicationFactory : WebApplicationFactory<DDDSample1.Program>
        {
            protected override void ConfigureWebHost(IWebHostBuilder builder)
            {
                // Force Testing environment so Startup will skip ReplaceService
                builder.UseEnvironment("Testing");

                // Ensure the content root points to the project directory so static
                // files and configuration are found correctly during tests.
                // AppContext.BaseDirectory is the test output (bin) folder; walk up
                // to the project root.
                var projectRoot = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", ".."));
                builder.UseContentRoot(projectRoot);

                // No extra service configuration here; Startup will register services
                // (including DbContext). We'll initialize the test database from the
                // test method after creating the factory client.
            }
        }
    }
}
