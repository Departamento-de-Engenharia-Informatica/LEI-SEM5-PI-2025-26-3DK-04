using System;
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
using System.Collections.Generic;
using System.IO;

namespace DDDSample1.Tests.System
{
    public class RepresentativeSystemTests : IClassFixture<WebApplicationFactory<DDDSample1.Program>>
    {
        private readonly WebApplicationFactory<DDDSample1.Program> _factory;
        private static readonly Random _rnd = new Random();

        public RepresentativeSystemTests()
        {
            _factory = new TestApplicationFactory();
        }

        [Fact]
        public async Task Representative_Full_Workflow_via_HttpApi()
        {
            var client = _factory.CreateClient();

            // IDs únicos
            var orgId = "ORG" + _rnd.Next(100000, 999999);
            var initialRepId = "CID" + _rnd.Next(100000, 999999);
            var newRepId = "CID" + _rnd.Next(100000, 999999);

            // 1) Create organization with initial representative
            var taxNumber = "PT" + _rnd.Next(10000000, 99999999) + "A";

            var orgDto = new OrganizationDto
            {
                Id = orgId,
                LegalName = "Org for Reps " + orgId,
                AlternativeName = "OrgRep",
                Address = "Org Street 1",
                TaxNumber = taxNumber,
                Representatives = new List<AddRepresentativeToOrgDto>()
                {
                    new AddRepresentativeToOrgDto
                    {
                        Name = "Initial Rep",
                        CitizenId = initialRepId,
                        Nationality = "PT",
                        Email = $"initial{Guid.NewGuid().ToString("N").Substring(0,4)}@gmail.com",
                        PhoneNumber = "9" + _rnd.Next(10000000, 99999999).ToString()
                    }
                }
            };

            var orgJson = JsonConvert.SerializeObject(orgDto);
            var orgResponse = await client.PostAsync("/api/Organizations", new StringContent(orgJson, Encoding.UTF8, "application/json"));
            var orgBody = await orgResponse.Content.ReadAsStringAsync();
            Console.WriteLine("Create Org Response: " + orgBody);

            orgResponse.StatusCode.Should().Be(HttpStatusCode.Created);

            var createdOrg = JsonConvert.DeserializeObject<OrganizationDto>(orgBody);
            createdOrg.Should().NotBeNull();
            var organizationIdForRep = createdOrg.Id;

            // 2) Create new representative
            var createDto = new AddRepresentativeDto
            {
                Name = "Bob Representative",
                CitizenId = newRepId,
                Nationality = "PT",
                Email = $"alice{Guid.NewGuid().ToString("N").Substring(0,4)}@gmail.com",
                PhoneNumber = "9" + _rnd.Next(10000000, 99999999).ToString(),
                OrganizationId = organizationIdForRep
            };

            var createJson = JsonConvert.SerializeObject(createDto);
            var createContent = new StringContent(createJson, Encoding.UTF8, "application/json");

            var createResponse = await client.PostAsync("/api/Representatives", createContent);
            var createBody = await createResponse.Content.ReadAsStringAsync();
            Console.WriteLine("Create Rep Response: " + createBody);

            createResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            var created = JsonConvert.DeserializeObject<RepresentativeDto>(createBody);
            created.Should().NotBeNull();
            created.CitizenId.Should().Be(newRepId);
            created.Status.Should().Be("Active");

            // 3) Get by id
            var getResponse = await client.GetAsync($"/api/Representatives/{created.CitizenId}");
            getResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            // 4) Update (mantendo os campos obrigatórios do DTO)
            var updateDto = new UpdateRepresentativeDto
            {
                CitizenId = created.CitizenId,
                Name = "Bob Updated",
                Email = $"bobupdated{Guid.NewGuid().ToString("N").Substring(0,4)}@gmail.com",
                PhoneNumber = createDto.PhoneNumber,
                Nationality = "PT"
            };

            var updateJson = JsonConvert.SerializeObject(updateDto);
            var updateContent = new StringContent(updateJson, Encoding.UTF8, "application/json");

            var updateResponse = await client.PutAsync($"/api/Representatives/{created.CitizenId}/update", updateContent);
            var updateBody = await updateResponse.Content.ReadAsStringAsync();
            Console.WriteLine("Update Rep Response: " + updateBody);

            updateResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            // 5) Deactivate
            var deactivateResponse = await client.PutAsync($"/api/Representatives/{created.CitizenId}/deactivate", null);
            deactivateResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            // 6) Activate
            var activateResponse = await client.PutAsync($"/api/Representatives/{created.CitizenId}/activate", null);
            activateResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            // 7) Attempt duplicate creation should fail
            var dupResponse = await client.PostAsync("/api/Representatives", createContent);
            dupResponse.StatusCode.Should().Be(HttpStatusCode.BadRequest);
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
