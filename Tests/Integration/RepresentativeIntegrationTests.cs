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
    public class RepresentativeIntegrationTests : IClassFixture<CustomWebApplicationFactory<DDDSample1.Program>>
    {
        private readonly HttpClient _client;
        private readonly CustomWebApplicationFactory<DDDSample1.Program> _factory;
        private static readonly Random _rand = new Random();

        public RepresentativeIntegrationTests(CustomWebApplicationFactory<DDDSample1.Program> factory)
        {
            _factory = factory;
            _client = factory.CreateClient();
        }
        
        [Fact]
        public async Task GetAll_ReturnsInsertedRepresentative()
        {
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();

            using (var scope = scopeFactory.CreateScope())
            {
                var repService = scope.ServiceProvider.GetRequiredService<RepresentativeService>();
                var orgRepo = scope.ServiceProvider.GetRequiredService<IOrganizationRepository>();

                var org = new Organization(
                    "Org123",
                    "Org Alice",
                    "Alt",
                    "Rua Velha",
                    "PT445566779"
                );
                await orgRepo.AddAsync(org);

                var dto = new AddRepresentativeDto
                {
                    Name = "Alice Rep",
                    CitizenId = "CID1001",
                    Nationality = "PT",
                    Email = "alice@gmail.com",
                    PhoneNumber = "911111111",
                    OrganizationId = org.Id.AsString()
                };

                await repService.AddRepresentativeAsync(dto);
            }

            var resp = await _client.GetAsync("/api/Representatives");
            resp.EnsureSuccessStatusCode();

            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<RepresentativeDto>>(str);

            list.Should().Contain(r => r.CitizenId == "CID1001" && r.Name == "Alice Rep");
        }

        [Fact]
        public async Task GetById_ReturnsCorrectRepresentative()
        {
            const string cid = "CID2002";
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();

            using (var scope = scopeFactory.CreateScope())
            {
                var repService = scope.ServiceProvider.GetRequiredService<RepresentativeService>();
                var orgRepo = scope.ServiceProvider.GetRequiredService<IOrganizationRepository>();

                var org = new Organization("ORG213213", "Org Bruno", "Alt", "Rua Nova", "PT443243243");
                await orgRepo.AddAsync(org);

                var dto = new AddRepresentativeDto
                {
                    Name = "Bruno Rep",
                    CitizenId = cid,
                    Nationality = "PT",
                    Email = "bruno@gmail.com",
                    PhoneNumber = "922222222",
                    OrganizationId = org.Id.AsString()
                };

                await repService.AddRepresentativeAsync(dto);
            }

            var resp = await _client.GetAsync($"/api/Representatives/{cid}");
            resp.EnsureSuccessStatusCode();

            var str = await resp.Content.ReadAsStringAsync();
            var dtoOut = JsonConvert.DeserializeObject<RepresentativeDto>(str);

            dtoOut.Should().NotBeNull();
            dtoOut.CitizenId.Should().Be(cid);
            dtoOut.Name.Should().Be("Bruno Rep");
        }

        [Fact]
        public async Task AddRepresentative_CreatesSuccessfully()
        {
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();

            var dto = new AddRepresentativeDto
            {
                Name = "Carla Rep",
                CitizenId = "CID3003",
                Nationality = "PT",
                Email = "carla@gmail.com",
                PhoneNumber = "933333333"
            };

            using (var scope = scopeFactory.CreateScope())
            {
                var orgRepo = scope.ServiceProvider.GetRequiredService<IOrganizationRepository>();
                var org = new Organization("ORG4681", "Org Carla", "Alt", "Rua Nova", "PT999777555");
                await orgRepo.AddAsync(org);
                dto.OrganizationId = org.Id.AsString();
            }

            var json = JsonConvert.SerializeObject(dto);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            var resp = await _client.PostAsync("/api/Representatives", content);
            resp.EnsureSuccessStatusCode();

            var body = await resp.Content.ReadAsStringAsync();
            var created = JsonConvert.DeserializeObject<RepresentativeDto>(body);

            created.Should().NotBeNull();
            created.CitizenId.Should().Be("CID3003");
            created.Status.Should().Be("Active");
        }

        [Fact]
        public async Task AddRepresentative_DuplicateEmail_ReturnsBadRequest()
        {
            const string email = "dup@gmail.com";
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();

            using (var scope = scopeFactory.CreateScope())
            {
                var repService = scope.ServiceProvider.GetRequiredService<RepresentativeService>();
                var orgRepo = scope.ServiceProvider.GetRequiredService<IOrganizationRepository>();

                var org = new Organization("ORG517", "Org Dup", "Alt", "Rua 1", "PT999777551");
                await orgRepo.AddAsync(org);

                var dto = new AddRepresentativeDto
                {
                    Name = "Diana Rep",
                    CitizenId = "CID4004",
                    Nationality = "PT",
                    Email = email,
                    PhoneNumber = "944444444",
                    OrganizationId = org.Id.AsString()
                };

                await repService.AddRepresentativeAsync(dto);
            }

            var dto2 = new AddRepresentativeDto
            {
                Name = "Dup Rep",
                CitizenId = "CID4005",
                Nationality = "PT",
                Email = email,
                PhoneNumber = "955555555"
            };

            using (var scope = scopeFactory.CreateScope())
            {
                var orgRepo = scope.ServiceProvider.GetRequiredService<IOrganizationRepository>();
                var org = new Organization("ORG901", "Org Dup2", "Alt", "Rua 2", "PT999777521");
                await orgRepo.AddAsync(org);
                dto2.OrganizationId = org.Id.AsString();
            }

            var json = JsonConvert.SerializeObject(dto2);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            var resp = await _client.PostAsync("/api/Representatives", content);
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.BadRequest);
        }

        [Fact]
        public async Task UpdateRepresentative_ChangesValues()
        {
            const string cid = "CID5005";
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();

            using (var scope = scopeFactory.CreateScope())
            {
                var repService = scope.ServiceProvider.GetRequiredService<RepresentativeService>();
                var orgRepo = scope.ServiceProvider.GetRequiredService<IOrganizationRepository>();

                var org = new Organization("ORG12389", "Org Eva", "Alt", "Rua Eva", "PT999771243");
                await orgRepo.AddAsync(org);

                var dto = new AddRepresentativeDto
                {
                    Name = "Eva Rep",
                    CitizenId = cid,
                    Nationality = "PT",
                    Email = "eva@gmail.com",
                    PhoneNumber = "966666666",
                    OrganizationId = org.Id.AsString()
                };

                await repService.AddRepresentativeAsync(dto);
            }

            var update = new AddRepresentativeDto
            {
                Name = "Eva Rep Updated",
                CitizenId = cid,
                Nationality = "ES",
                Email = "eva.new@gmail.com",
                PhoneNumber = "977777777"
            };

            var json = JsonConvert.SerializeObject(update);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            var resp = await _client.PutAsync($"/api/Representatives/{cid}/update", content);
            resp.EnsureSuccessStatusCode();

            var body = await resp.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<RepresentativeDto>(body);
            updated.Nationality.Should().Be("ES");
            updated.Email.Should().Be("eva.new@gmail.com");
        }

        [Fact]
        public async Task DeactivateRepresentative_SetsStatusInactive()
        {
            const string cid = "CID6006";
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();

            using (var scope = scopeFactory.CreateScope())
            {
                var repService = scope.ServiceProvider.GetRequiredService<RepresentativeService>();
                var orgRepo = scope.ServiceProvider.GetRequiredService<IOrganizationRepository>();

                var org = new Organization("ORGA6781", "Org Fred", "Alt", "Rua Fred", "PT918877556");
                await orgRepo.AddAsync(org);

                var dto = new AddRepresentativeDto
                {
                    Name = "Fred Rep",
                    CitizenId = cid,
                    Nationality = "PT",
                    Email = "fred@gmail.com",
                    PhoneNumber = "988888888",
                    OrganizationId = org.Id.AsString()
                };

                await repService.AddRepresentativeAsync(dto);
            }

            var resp = await _client.PutAsync($"/api/Representatives/{cid}/deactivate", null);
            resp.EnsureSuccessStatusCode();

            var str = await resp.Content.ReadAsStringAsync();
            var dtoOut = JsonConvert.DeserializeObject<RepresentativeDto>(str);
            dtoOut.Status.Should().Be("Inactive");
        }

        [Fact]
        public async Task ActivateRepresentative_SetsStatusActive()
        {
            const string cid = "CID7007";
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();

            using (var scope = scopeFactory.CreateScope())
            {
                var repService = scope.ServiceProvider.GetRequiredService<RepresentativeService>();
                var orgRepo = scope.ServiceProvider.GetRequiredService<IOrganizationRepository>();

                var org = new Organization("ORGA225667", "Org Gina", "Alt", "Rua Gina", "PT918872257");
                await orgRepo.AddAsync(org);

                var dto = new AddRepresentativeDto
                {
                    Name = "Gina Rep",
                    CitizenId = cid,
                    Nationality = "PT",
                    Email = "gina@gmail.com",
                    PhoneNumber = "999999999",
                    OrganizationId = org.Id.AsString()
                };

                await repService.AddRepresentativeAsync(dto);
                await repService.DeactivateRepresentativeAsync(cid);
            }

            var resp = await _client.PutAsync($"/api/Representatives/{cid}/activate", null);
            resp.EnsureSuccessStatusCode();

            var str = await resp.Content.ReadAsStringAsync();
            var dtoOut = JsonConvert.DeserializeObject<RepresentativeDto>(str);
            dtoOut.Status.Should().Be("Active");
        }

        [Fact]
        public async Task GetById_NotFound_Returns404()
        {
            var resp = await _client.GetAsync("/api/Representatives/CID9999");
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.NotFound);
        }
    }
}
