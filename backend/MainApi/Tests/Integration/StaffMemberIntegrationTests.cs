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
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Qualifications;

namespace DDDNetCore.Tests.Integration
{
    public class StaffMemberIntegrationTests : IClassFixture<CustomWebApplicationFactory<DDDSample1.Program>>
    {
        private readonly HttpClient _client;
        private readonly CustomWebApplicationFactory<DDDSample1.Program> _factory;

        public StaffMemberIntegrationTests(CustomWebApplicationFactory<DDDSample1.Program> factory)
        {
            _factory = factory;
            _client = factory.CreateClient();
        }

        [Fact]
        public async Task GetAll_ReturnsInsertedStaffMember()
        {
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var staff = new StaffMember("John Doe","john@example.com",912345678,"08:00-17:00");
                db.StaffMembers.Add(staff);
                db.SaveChanges();
            }

            var resp = await _client.GetAsync("/api/StaffMembers");
            resp.EnsureSuccessStatusCode();
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<StaffMemberDto>>(str);
            list.Should().NotBeNull();
            list.Should().ContainSingle(s => s.Name == "John Doe");
        }

        [Fact]
        public async Task GetById_ReturnsCorrectStaffMember()
        {
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var staff = new StaffMember("Jane Specific","jane@example.com",912345679,"09:00-18:00");
                db.StaffMembers.Add(staff);
                db.SaveChanges();
                insertedId = staff.Id.AsGuid();
            }

            var resp = await _client.GetAsync($"/api/StaffMembers/{insertedId}");
            resp.EnsureSuccessStatusCode();
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var str = await resp.Content.ReadAsStringAsync();
            var dto = JsonConvert.DeserializeObject<StaffMemberDto>(str);
            dto.Should().NotBeNull();
            dto.Id.Should().Be(insertedId);
            dto.Name.Should().Be("Jane Specific");
        }

        [Fact]
        public async Task Create_AddsStaffMember()
        {
            var createDto = new CreateStaffMemberDto { Name = "Created Staff", Email = "cstaff@example.com", PhoneNumber = 912345680, OperationalWindow = "08:00-16:00" };
            var jsonContent = JsonConvert.SerializeObject(createDto);
            var content = new StringContent(jsonContent, Encoding.UTF8, "application/json");

            var response = await _client.PostAsync("/api/StaffMembers", content);

            response.StatusCode.Should().Be(System.Net.HttpStatusCode.Created);
            var responseBody = await response.Content.ReadAsStringAsync();
            responseBody.Should().NotBeNullOrWhiteSpace();
            var created = JsonConvert.DeserializeObject<StaffMemberDto>(responseBody);
            created.Should().NotBeNull();
            created.Name.Should().Be("Created Staff");
            created.Id.Should().NotBe(Guid.Empty);
            created.Status.Should().Be(MemberStatus.Available);
        }

        [Fact]
        public async Task Update_ChangesStaffMember()
        {
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var staff = new StaffMember("Old Name","old@example.com",912345681,"08:00-12:00");
                db.StaffMembers.Add(staff);
                db.SaveChanges();
                insertedId = staff.Id.AsGuid();
            }

            var updateDto = new UpdateStaffMemberDto { Name = "New Name" };
            var jsonContent = JsonConvert.SerializeObject(updateDto);
            var content = new StringContent(jsonContent, Encoding.UTF8, "application/json");

            var response = await _client.PutAsync($"/api/StaffMembers/{insertedId}", content);
            response.EnsureSuccessStatusCode();
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var responseBody = await response.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<StaffMemberDto>(responseBody);
            updated.Should().NotBeNull();
            updated.Id.Should().Be(insertedId);
            updated.Name.Should().Be("New Name");
        }

        [Fact]
        public async Task Deactivate_And_Reactivate_StaffMember()
        {
            Guid insertedId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var staff = new StaffMember("To Deactivate","td@example.com",912345682,"08:00-17:00");
                db.StaffMembers.Add(staff);
                db.SaveChanges();
                insertedId = staff.Id.AsGuid();
            }

            // Deactivate (DELETE)
            var response = await _client.DeleteAsync($"/api/StaffMembers/{insertedId}");
            response.EnsureSuccessStatusCode();
            response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var responseBody = await response.Content.ReadAsStringAsync();
            var deactivated = JsonConvert.DeserializeObject<StaffMemberDto>(responseBody);
            deactivated.Should().NotBeNull();
            deactivated.Status.Should().Be(MemberStatus.Unavailable);

            // Ensure not in active list
            var allResp = await _client.GetAsync("/api/StaffMembers");
            allResp.EnsureSuccessStatusCode();
            var allStr = await allResp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<StaffMemberDto>>(allStr);
            list.Should().NotContain(s => s.Id == insertedId);

            // Reactivate
            var reactResp = await _client.PutAsync($"/api/StaffMembers/{insertedId}/reactivate", null);
            reactResp.EnsureSuccessStatusCode();
            var reactStr = await reactResp.Content.ReadAsStringAsync();
            var reactivated = JsonConvert.DeserializeObject<StaffMemberDto>(reactStr);
            reactivated.Should().NotBeNull();
            reactivated.Status.Should().Be(MemberStatus.Available);

            // Back in active list
            var allResp2 = await _client.GetAsync("/api/StaffMembers");
            allResp2.EnsureSuccessStatusCode();
            var allStr2 = await allResp2.Content.ReadAsStringAsync();
            var list2 = JsonConvert.DeserializeObject<List<StaffMemberDto>>(allStr2);
            list2.Should().Contain(s => s.Id == insertedId);
        }

        [Fact]
        public async Task AddAndRemoveQualification_Works()
        {
            Guid staffId;
            Guid qualId;
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                var qual = new Qualification("Sample Qualification Name");
                db.Qualifications.Add(qual);
                var staff = new StaffMember("Qualified","q@example.com",912345683,"08:00-17:00");
                db.StaffMembers.Add(staff);
                db.SaveChanges();
                staffId = staff.Id.AsGuid();
                qualId = qual.Id.AsGuid();
            }

            var addDto = new AddQualificationDto { QualificationId = qualId };
            var content = new StringContent(JsonConvert.SerializeObject(addDto), Encoding.UTF8, "application/json");
            var resp = await _client.PostAsync($"/api/StaffMembers/{staffId}/qualifications", content);
            resp.EnsureSuccessStatusCode();
            var str = await resp.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<StaffMemberDto>(str);
            updated.Should().NotBeNull();
            updated.Qualifications.Should().Contain(q => q.Id == qualId);

            // Remove qualification
            var delResp = await _client.DeleteAsync($"/api/StaffMembers/{staffId}/qualifications/{qualId}");
            delResp.EnsureSuccessStatusCode();
            var delStr = await delResp.Content.ReadAsStringAsync();
            var after = JsonConvert.DeserializeObject<StaffMemberDto>(delStr);
            after.Should().NotBeNull();
            after.Qualifications.Should().NotContain(q => q.Id == qualId);
        }

        [Fact]
        public async Task SearchByName_ReturnsMatches()
        {
            var scopeFactory = _factory.Services.GetRequiredService<IServiceScopeFactory>();
            using (var scope = scopeFactory.CreateScope())
            {
                var db = scope.ServiceProvider.GetService<DDDSample1DbContext>();
                db.StaffMembers.Add(new StaffMember("Alice Tester","a@test.com",912345684,"08:00-12:00"));
                db.StaffMembers.Add(new StaffMember("Bob Tester","b@test.com",912345685,"08:00-12:00"));
                db.StaffMembers.Add(new StaffMember("Charlie","c@test.com",912345686,"08:00-12:00"));
                db.SaveChanges();
            }

            var resp = await _client.GetAsync("/api/StaffMembers/search?name=Tester");
            resp.EnsureSuccessStatusCode();
            resp.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);
            var str = await resp.Content.ReadAsStringAsync();
            var list = JsonConvert.DeserializeObject<List<StaffMemberDto>>(str);
            list.Should().NotBeNull();
            list.Should().OnlyContain(s => s.Name.Contains("Tester"));
        }
    }
}
