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
            var unique = Guid.NewGuid().ToString("N").Substring(0, 6);

            using (var scope = scopeFactory.CreateScope())
            {
                var repService = scope.ServiceProvider.GetRequiredService<RepresentativeService>();
                var orgRepo = scope.ServiceProvider.GetRequiredService<IOrganizationRepository>();

                var org = new Organization(
                    "ORG" + unique,
                    "Org Alice " + unique,
                    "Alt",
                    "Rua Velha",
                    "PT" + _rand.Next(10000000, 99999999)
                );
                await orgRepo.AddAsync(org);

                var dto = new AddRepresentativeDto
                {
                    Name = "Alice Rep " + unique,
                    CitizenId = "CID" + unique,
                    Nationality = "PT",
                    Email = $"alice{unique}@gmail.com",
                    PhoneNumber = "9" + _rand.Next(10000000, 99999999),
                    OrganizationId = org.Id.AsString()
                };

                await repService.AddRepresentativeAsync(dto);
            }

            var resp = await _client.GetAsync("/api/Representatives");
            resp.EnsureSuccessStatusCode();

            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<RepresentativeDto>>(str);

            list.Should().Contain(r => r.Name.StartsWith("Alice Rep"));
        }

        [Fact]
        public async Task GetById_ReturnsCorrectRepresentative()
        {
            var unique = Guid.NewGuid().ToString("N").Substring(0, 6);
            var cid = "CID" + unique;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();

            using (var scope = scopeFactory.CreateScope())
            {
                var repService = scope.ServiceProvider.GetRequiredService<RepresentativeService>();
                var orgRepo = scope.ServiceProvider.GetRequiredService<IOrganizationRepository>();

                var org = new Organization("ORG" + unique, "Org Bruno " + unique, "Alt", "Rua Nova", "PT" + _rand.Next(10000000, 99999999));
                await orgRepo.AddAsync(org);

                var dto = new AddRepresentativeDto
                {
                    Name = "Bruno Rep " + unique,
                    CitizenId = cid,
                    Nationality = "PT",
                    Email = $"bruno{unique}@gmail.com",
                    PhoneNumber = "9" + _rand.Next(10000000, 99999999),
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
            dtoOut.Name.Should().StartWith("Bruno Rep");
        }

        [Fact]
        public async Task AddRepresentative_CreatesSuccessfully()
        {
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            var unique = Guid.NewGuid().ToString("N").Substring(0, 6);

            var dto = new AddRepresentativeDto
            {
                Name = "Carla Rep " + unique,
                CitizenId = "CID" + unique,
                Nationality = "PT",
                Email = $"carla{unique}@gmail.com",
                PhoneNumber = "9" + _rand.Next(10000000, 99999999)
            };

            using (var scope = scopeFactory.CreateScope())
            {
                var orgRepo = scope.ServiceProvider.GetRequiredService<IOrganizationRepository>();
                var org = new Organization("ORG" + unique, "Org Carla", "Alt", "Rua Nova", "PT" + _rand.Next(10000000, 99999999));
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
            created.CitizenId.Should().Be(dto.CitizenId);
            created.Status.Should().Be("Active");
        }

        [Fact]
        public async Task AddRepresentative_DuplicateEmail_ReturnsBadRequest()
        {
            var unique = Guid.NewGuid().ToString("N").Substring(0, 6);
            var email = $"dup{unique}@gmail.com";
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();

            using (var scope = scopeFactory.CreateScope())
            {
                var repService = scope.ServiceProvider.GetRequiredService<RepresentativeService>();
                var orgRepo = scope.ServiceProvider.GetRequiredService<IOrganizationRepository>();

                var org = new Organization("ORG" + unique, "Org Dup", "Alt", "Rua 1", "PT" + _rand.Next(10000000, 99999999));
                await orgRepo.AddAsync(org);

                var dto = new AddRepresentativeDto
                {
                    Name = "Diana Rep " + unique,
                    CitizenId = "CID" + unique,
                    Nationality = "PT",
                    Email = email,
                    PhoneNumber = "9" + _rand.Next(10000000, 99999999),
                    OrganizationId = org.Id.AsString()
                };

                await repService.AddRepresentativeAsync(dto);
            }

            var dto2 = new AddRepresentativeDto
            {
                Name = "Dup Rep " + unique,
                CitizenId = "CID" + Guid.NewGuid().ToString("N").Substring(0, 6),
                Nationality = "PT",
                Email = email, // mesmo email
                PhoneNumber = "9" + _rand.Next(10000000, 99999999)
            };

            using (var scope = scopeFactory.CreateScope())
            {
                var orgRepo = scope.ServiceProvider.GetRequiredService<IOrganizationRepository>();
                var org = new Organization("ORG" + Guid.NewGuid().ToString("N").Substring(0, 6), "Org Dup2", "Alt", "Rua 2", "PT" + _rand.Next(10000000, 99999999));
                await orgRepo.AddAsync(org);
                dto2.OrganizationId = org.Id.AsString();
            }

            var json = JsonConvert.SerializeObject(dto2);
            var content = new StringContent(json, Encoding.UTF8, "application/json");

            var resp = await _client.PostAsync("/api/Representatives", content);
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.BadRequest);
        }

        // restantes testes (Update, Activate, Deactivate, NotFound) mantêm-se iguais
    }
}
*/