using System;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;
using FluentAssertions;
using Microsoft.AspNetCore.Hosting;
using System.IO;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.DependencyInjection;
using Newtonsoft.Json;
using Xunit;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Qualifications;

namespace DDDSample1.Tests.System
{
    public class StaffMemberSystemTests : IClassFixture<WebApplicationFactory<DDDSample1.Program>>
    {
        private readonly WebApplicationFactory<DDDSample1.Program> _factory;

        public StaffMemberSystemTests()
        {
            // Create a factory that forces the app environment to "Testing"
            _factory = new TestApplicationFactory();
        }

        [Fact]
        public async Task StaffMember_CRUD_and_Qualifications_Workflow_via_HttpApi()
        {
            var client = _factory.CreateClient();

            // 1) Create staff member
            var createDto = new CreateStaffMemberDto { Name = "System Staff", Email = "sys@example.com", PhoneNumber = 912345699, OperationalWindow = "08:00-16:00" };
            var createJson = JsonConvert.SerializeObject(createDto);
            var createContent = new StringContent(createJson, Encoding.UTF8, "application/json");

            var createResponse = await client.PostAsync("/api/StaffMembers", createContent);
            createResponse.StatusCode.Should().Be(HttpStatusCode.Created);

            var createdBody = await createResponse.Content.ReadAsStringAsync();
            var created = JsonConvert.DeserializeObject<StaffMemberDto>(createdBody);
            created.Should().NotBeNull();
            created.Id.Should().NotBe(Guid.Empty);
            created.Name.Should().Be(createDto.Name);
            created.Status.Should().Be(MemberStatus.Available);

            // 2) Get by id
            var getResponse = await client.GetAsync($"/api/StaffMembers/{created.Id}");
            getResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var getBody = await getResponse.Content.ReadAsStringAsync();
            var got = JsonConvert.DeserializeObject<StaffMemberDto>(getBody);
            got.Should().NotBeNull();
            got.Id.Should().Be(created.Id);

            // 3) Update
            var updateDto = new UpdateStaffMemberDto { Name = "System Staff Updated" };
            var updateJson = JsonConvert.SerializeObject(updateDto);
            var updateContent = new StringContent(updateJson, Encoding.UTF8, "application/json");
            var updateResponse = await client.PutAsync($"/api/StaffMembers/{created.Id}", updateContent);
            updateResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var updatedBody = await updateResponse.Content.ReadAsStringAsync();
            var updated = JsonConvert.DeserializeObject<StaffMemberDto>(updatedBody);
            updated.Should().NotBeNull();
            updated.Name.Should().Be(updateDto.Name);

            // 4) Get all and ensure contains
            var allResponse = await client.GetAsync("/api/StaffMembers");
            allResponse.StatusCode.Should().Be(HttpStatusCode.OK);
            var allBody = await allResponse.Content.ReadAsStringAsync();
            var all = JsonConvert.DeserializeObject<StaffMemberDto[]>(allBody);
            all.Should().NotBeNull();
            all.Select(s => s.Id).Should().Contain(created.Id);

            // 5) Create a qualification via API then add to staff
            var createQ = new CreateQualificationDto { Name = "System Staff Qual" };
            var createQJson = JsonConvert.SerializeObject(createQ);
            var createQContent = new StringContent(createQJson, Encoding.UTF8, "application/json");
            var createQResp = await client.PostAsync("/api/Qualifications", createQContent);
            createQResp.StatusCode.Should().Be(HttpStatusCode.Created);
            var qBody = await createQResp.Content.ReadAsStringAsync();
            var qCreated = JsonConvert.DeserializeObject<QualificationDto>(qBody);
            qCreated.Should().NotBeNull();

            // Add qualification to staff
            var addDto = new AddQualificationDto { QualificationId = qCreated.Id };
            var addContent = new StringContent(JsonConvert.SerializeObject(addDto), Encoding.UTF8, "application/json");
            var addResp = await client.PostAsync($"/api/StaffMembers/{created.Id}/qualifications", addContent);
            addResp.StatusCode.Should().Be(HttpStatusCode.OK);
            var addBody = await addResp.Content.ReadAsStringAsync();
            var afterAdd = JsonConvert.DeserializeObject<StaffMemberDto>(addBody);
            afterAdd.Should().NotBeNull();
            afterAdd.Qualifications.Should().Contain(q => q.Id == qCreated.Id);

            // 6) Remove qualification
            var delResp = await client.DeleteAsync($"/api/StaffMembers/{created.Id}/qualifications/{qCreated.Id}");
            delResp.StatusCode.Should().Be(HttpStatusCode.OK);
            var delBody = await delResp.Content.ReadAsStringAsync();
            var afterDel = JsonConvert.DeserializeObject<StaffMemberDto>(delBody);
            afterDel.Should().NotBeNull();
            afterDel.Qualifications.Should().NotContain(q => q.Id == qCreated.Id);

            // 7) Deactivate staff (DELETE) -> should set status Unavailable and not appear in /api/StaffMembers
            var delStaffResp = await client.DeleteAsync($"/api/StaffMembers/{created.Id}");
            delStaffResp.StatusCode.Should().Be(HttpStatusCode.OK);
            var delStaffBody = await delStaffResp.Content.ReadAsStringAsync();
            var deactivated = JsonConvert.DeserializeObject<StaffMemberDto>(delStaffBody);
            deactivated.Should().NotBeNull();
            deactivated.Status.Should().Be(MemberStatus.Unavailable);

            var activeListResp = await client.GetAsync("/api/StaffMembers");
            activeListResp.StatusCode.Should().Be(HttpStatusCode.OK);
            var activeListBody = await activeListResp.Content.ReadAsStringAsync();
            var actives = JsonConvert.DeserializeObject<StaffMemberDto[]>(activeListBody);
            actives.Should().NotContain(s => s.Id == created.Id);

            // 8) Reactivate
            var reactResp = await client.PutAsync($"/api/StaffMembers/{created.Id}/reactivate", null);
            reactResp.StatusCode.Should().Be(HttpStatusCode.OK);
            var reactBody = await reactResp.Content.ReadAsStringAsync();
            var reactivated = JsonConvert.DeserializeObject<StaffMemberDto>(reactBody);
            reactivated.Should().NotBeNull();
            reactivated.Status.Should().Be(MemberStatus.Available);

            // Ensure in active list again
            var activeListResp2 = await client.GetAsync("/api/StaffMembers");
            activeListResp2.StatusCode.Should().Be(HttpStatusCode.OK);
            var activeListBody2 = await activeListResp2.Content.ReadAsStringAsync();
            var actives2 = JsonConvert.DeserializeObject<StaffMemberDto[]>(activeListBody2);
            actives2.Select(s => s.Id).Should().Contain(created.Id);
        }

        private class TestApplicationFactory : WebApplicationFactory<DDDSample1.Program>
        {
            protected override void ConfigureWebHost(IWebHostBuilder builder)
            {
                // Force Testing environment so Startup will skip ReplaceService
                builder.UseEnvironment("Testing");

                // Ensure the content root points to the project directory so static
                // files and configuration are found correctly during tests.
                var projectRoot = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", ".."));
                builder.UseContentRoot(projectRoot);

                // No extra service configuration here; Startup will register services
                // (including DbContext). We'll use the API to create initial data.
            }
        }
    }
}
