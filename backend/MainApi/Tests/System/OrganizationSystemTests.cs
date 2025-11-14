/*
using System;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using FluentAssertions;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc.Testing;
using Newtonsoft.Json;
using Xunit;
using DDDSample1.Domain.Organizations;

namespace DDDSample1.Tests.System
{
    public class OrganizationSystemTests : IClassFixture<WebApplicationFactory<DDDSample1.Program>>
    {
        private readonly WebApplicationFactory<DDDSample1.Program> _factory;

        public OrganizationSystemTests()
        {
            _factory = new TestApplicationFactory();
        }

        [Fact]
        public async Task Organization_Full_Workflow_via_HttpApi()
        {
            var client = _factory.CreateClient();

            // IDs únicos usando Guid para evitar conflito
            var orgId = "ORG" + new Random().Next(100000, 999999);
            var repId = "CID" + new Random().Next(100000, 999999);
            
            // 1) Create
            var createDto = new OrganizationDto
            {
                Id = orgId,
                LegalName = "Test Org " + Guid.NewGuid(),
                AlternativeName = "TOrg",
                Address = "123 Main St",
                TaxNumber = "PT" + new Random().Next(10000000,99999999), 
                Representatives = new[]
                {
                    new AddRepresentativeToOrgDto
                    {
                        Name = "Alice Representative",
                        CitizenId = repId,
                        Nationality = "PT",
                        Email = $"alice{Guid.NewGuid().ToString("N").Substring(0,4)}@gmail.com",
                        PhoneNumber = "9" + new Random().Next(10000000,99999999) // número válido
                    }
                }.ToList()
            };

            var createJson = JsonConvert.SerializeObject(createDto);
            var createContent = new StringContent(createJson, Encoding.UTF8, "application/json");

            var createResponse = await client.PostAsync("/api/Organizations", createContent);
            var createBody = await createResponse.Content.ReadAsStringAsync();
            Console.WriteLine("Create Org Response: " + createBody); // ajuda no debug

            createResponse.StatusCode.Should().Be(HttpStatusCode.Created);

            var created = JsonConvert.DeserializeObject<OrganizationDto>(createBody);
            created.Should().NotBeNull();
            created.Id.Should().Be(createDto.Id);
            created.Representatives.Should().HaveCount(1);

            // 2) Get by id
            var getResponse = await client.GetAsync($"/api/Organizations/{created.Id}");
            getResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            // 3) Get all
            var allResponse = await client.GetAsync("/api/Organizations");
            allResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            // 4) Attempt duplicate creation should fail
            var duplicateResponse = await client.PostAsync("/api/Organizations", createContent);
            duplicateResponse.StatusCode.Should().Be(HttpStatusCode.BadRequest);
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
*/