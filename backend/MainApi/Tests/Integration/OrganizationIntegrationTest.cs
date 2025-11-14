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
using DDDSample1.Domain.Organizations;

namespace DDDNetCore.Tests.Integration
{
    public class OrganizationIntegrationTests : IClassFixture<CustomWebApplicationFactory<DDDSample1.Program>>
    {
        private readonly HttpClient _client;
        private readonly CustomWebApplicationFactory<DDDSample1.Program> _factory;

        public OrganizationIntegrationTests(CustomWebApplicationFactory<DDDSample1.Program> factory)
        {
            _factory = factory;
            _client = factory.CreateClient();
        }

        [Fact]
        public async Task GetAll_ReturnsInsertedOrganization()
        {
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();

                var org = new Organization(
                    id: "ORG001",
                    legalName: "Legal Org A",
                    alternativeName: "Alt Org A",
                    address: "Rua das Oliveiras, 123",
                    taxNumber: "PT123456789");

                var rep = new Representative("Alice", "CID001", "PT", "alice@gmail.com", "911111111");
                org.AddRepresentative(rep);
                db.Organizations.Add(org);
                db.SaveChanges();
            }

            var resp = await _client.GetAsync("/api/Organizations");
            resp.EnsureSuccessStatusCode();
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);

            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<OrganizationDto>>(str);
            list.Should().NotBeNull();
            list.Should().Contain(o => o.Id == "ORG001" && o.LegalName == "Legal Org A");
        }

        [Fact]
        public async Task GetById_ReturnsCorrectOrganization()
        {
            const string orgId = "ORG002";
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();

                var org = new Organization(
                    id: orgId,
                    legalName: "Legal Org B",
                    alternativeName: "Alt Org B",
                    address: "Av. Central, 100",
                    taxNumber: "PT987654321");

                var rep = new Representative("Bruno", "CID002", "PT", "bruno@gmail.com", "922222222");
                org.AddRepresentative(rep);
                db.Organizations.Add(org);
                db.SaveChanges();
            }

            var resp = await _client.GetAsync($"/api/Organizations/{orgId}");
            resp.EnsureSuccessStatusCode();
            var str = await resp.Content.ReadAsStringAsync();
            var dto = JsonConvert.DeserializeObject<OrganizationDto>(str);

            dto.Should().NotBeNull();
            dto.Id.Should().Be(orgId);
            dto.LegalName.Should().Be("Legal Org B");
            dto.Representatives.Should().ContainSingle(r => r.Email == "bruno@gmail.com");
        }

        [Fact]
        public async Task Register_CreatesNewOrganization()
        {
            var newOrg = new OrganizationDto
            {
                Id = "ORG003",
                LegalName = "Legal Org C",
                AlternativeName = "Alt Org C",
                Address = "Rua Nova, 456",
                TaxNumber = "PT112233445",
                Representatives = new List<AddRepresentativeToOrgDto>
                {
                    new AddRepresentativeToOrgDto
                    {
                        Name = "Carla",
                        CitizenId = "CID003",
                        Nationality = "PT",
                        Email = "carla@gmail.com",
                        PhoneNumber = "933333333"
                    }
                }
            };

            var jsonContent = JsonConvert.SerializeObject(newOrg);
            var content = new StringContent(jsonContent, Encoding.UTF8, "application/json");

            var response = await _client.PostAsync("/api/Organizations", content);
            response.EnsureSuccessStatusCode();
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.Created);

            var body = await response.Content.ReadAsStringAsync();
            var created = JsonConvert.DeserializeObject<OrganizationDto>(body);

            created.Should().NotBeNull();
            created.Id.Should().Be("ORG003");
            created.LegalName.Should().Be("Legal Org C");
            created.Representatives.Should().ContainSingle(r => r.Email == "carla@gmail.com");
        }

        [Fact]
        public async Task Register_WithoutRepresentatives_ReturnsBadRequest()
        {
            var invalidOrg = new OrganizationDto
            {
                Id = "ORG004",
                LegalName = "Org Sem Rep",
                AlternativeName = null,
                Address = "Rua 123",
                TaxNumber = "PT445566778",
                Representatives = new List<AddRepresentativeToOrgDto>()
            };

            var jsonContent = JsonConvert.SerializeObject(invalidOrg);
            var content = new StringContent(jsonContent, Encoding.UTF8, "application/json");

            var response = await _client.PostAsync("/api/Organizations", content);
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.BadRequest);

            var body = await response.Content.ReadAsStringAsync();
            body.Should().Contain("must have at least one representative");
        }

        [Fact]
        public async Task Register_WithDuplicateId_ReturnsBadRequest()
        {
            const string orgId = "ORG005";
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var org = new Organization(orgId, "Existing Org", "Alt", "Rua Velha", "PT445566779");
                var rep = new Representative("Diana", "CID005", "PT", "diana@gmail.com", "944444444");
                org.AddRepresentative(rep);
                db.Organizations.Add(org);
                db.SaveChanges();
            }

            var newOrg = new OrganizationDto
            {
                Id = orgId,
                LegalName = "Duplicate Org",
                AlternativeName = "Dup",
                Address = "Rua Dup, 123",
                TaxNumber = "PT445566770",
                Representatives = new List<AddRepresentativeToOrgDto>
                {
                    new AddRepresentativeToOrgDto
                    {
                        Name = "Duarte",
                        CitizenId = "CID006",
                        Nationality = "PT",
                        Email = "duarte@gmail.com",
                        PhoneNumber = "955555555"
                    }
                }
            };

            var jsonContent = JsonConvert.SerializeObject(newOrg);
            var content = new StringContent(jsonContent, Encoding.UTF8, "application/json");

            var response = await _client.PostAsync("/api/Organizations", content);
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.BadRequest);

            var body = await response.Content.ReadAsStringAsync();
            body.Should().Contain("already exists");
        }
    }
}
*/