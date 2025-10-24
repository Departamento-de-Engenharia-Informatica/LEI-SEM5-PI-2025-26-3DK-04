using System;
using System.Collections.Generic;
using System.Net;
using System.Net.Http;
using System.Net.Http.Json;
using System.Threading.Tasks;
using DDDSample1.Domain.Vessels.VesselInformation;
using DDDSample1.Domain.Vessels;
using DDDSample1.Domain.Organizations;
using DDDSample1.Infrastructure;
using DDDSample1.Domain.Vessels.VesselVisitNotification;
using FluentAssertions;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.EntityFrameworkCore;
using Newtonsoft.Json;
using Xunit;

namespace DDDNetCore.Tests.Integration
{
    public class VesselVisitNotificationIntegrationTests : IClassFixture<CustomWebApplicationFactory<DDDSample1.Program>>
    {
        private readonly CustomWebApplicationFactory<DDDSample1.Program> _factory;

        public VesselVisitNotificationIntegrationTests(CustomWebApplicationFactory<DDDSample1.Program> factory)
        {
            _factory = factory;
        }

        private CargoManifest CreateManifestWithContainer(double weight)
        {
            var manifest = CargoManifest.Create(Guid.NewGuid().ToString());
            var container = Container.Create(Guid.NewGuid().ToString(), weight, "test");
            manifest.AddContainer(container);
            return manifest;
        }

        [Fact]
        public async Task CreateNotification_ReturnsCreatedAndCanBeRetrieved()
        {
            // Arrange
            var client = _factory.CreateClient();

            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<DDDSample1DbContext>();

                // Seed VesselType
                var vt = new VesselType("TypeA","desc",2000,1,1,1);
                db.VesselTypes.Add(vt);

                // Seed Vessel
                var vessel = new Vessel("IMO1234567","TestVessel", vt.Id, "owner", "operator");
                db.Vessels.Add(vessel);

                // Seed Representative
                var rep = new Representative("Rep Name","rep-123","PT","rep@example.com","123456789");
                db.Representatives.Add(rep);

                await db.SaveChangesAsync();

                // Build payload
                var payload = new
                {
                    VesselId = vessel.Id.AsGuid(),
                    RepresentativeId = rep.Id.AsString(),
                    LoadingManifests = new List<CargoManifest> { CreateManifestWithContainer(1000) },
                    UnloadingManifests = new List<CargoManifest> { CreateManifestWithContainer(500) }
                };

                // Act
                var response = await client.PostAsJsonAsync("/api/VesselVisitNotifications", payload);

                // Assert
                response.StatusCode.Should().Be(HttpStatusCode.Created);

                var content = await response.Content.ReadAsStringAsync();
                var dto = JsonConvert.DeserializeObject<dynamic>(content);
                ((Guid)dto.id).Should().NotBe(Guid.Empty);
                ((Guid)dto.vesselId).Should().Be(vessel.Id.AsGuid());
            }
        }

        [Fact]
        public async Task SubmitEndpoint_ChangesStatusToSubmitted()
        {
            var client = _factory.CreateClient();

            Guid notifId;

            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<DDDSample1DbContext>();

                var vt = new VesselType("TypeB","desc",2000,1,1,1);
                db.VesselTypes.Add(vt);
                var vessel = new Vessel("IMO1234568","V2", vt.Id, "owner", "operator");
                db.Vessels.Add(vessel);
                var rep = new Representative("Rep Name","rep-456","PT","rep2@example.com","987654321");
                db.Representatives.Add(rep);
                await db.SaveChangesAsync();

                var payload = new
                {
                    VesselId = vessel.Id.AsGuid(),
                    RepresentativeId = rep.Id.AsString(),
                    LoadingManifests = new List<CargoManifest> { CreateManifestWithContainer(100) },
                    UnloadingManifests = new List<CargoManifest> { CreateManifestWithContainer(50) }
                };

                var createResp = await client.PostAsJsonAsync("/api/VesselVisitNotifications", payload);
                createResp.StatusCode.Should().Be(HttpStatusCode.Created);
                var created = JsonConvert.DeserializeObject<dynamic>(await createResp.Content.ReadAsStringAsync());
                notifId = (Guid)created.id;
            }

            // Submit
            var submitResp = await client.PutAsync($"/api/VesselVisitNotifications/{notifId}/submit", null);
            submitResp.StatusCode.Should().Be(HttpStatusCode.OK);

            var body = await submitResp.Content.ReadAsStringAsync();
            var dto = JsonConvert.DeserializeObject<dynamic>(body);
            string status = dto.status;
            status.Should().Be("Submitted");
        }

        [Fact]
        public async Task WithdrawAndResumeFlow_Works()
        {
            var client = _factory.CreateClient();

            Guid notifId;

            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<DDDSample1DbContext>();

                var vt = new VesselType("TypeC","desc",2000,1,1,1);
                db.VesselTypes.Add(vt);
                var vessel = new Vessel("IMO1234569","V3", vt.Id, "owner", "operator");
                db.Vessels.Add(vessel);
                var rep = new Representative("Rep Name","rep-789","PT","rep3@example.com","111222333");
                db.Representatives.Add(rep);
                await db.SaveChangesAsync();

                var payload = new
                {
                    VesselId = vessel.Id.AsGuid(),
                    RepresentativeId = rep.Id.AsString(),
                    LoadingManifests = new List<CargoManifest> { CreateManifestWithContainer(10) },
                    UnloadingManifests = new List<CargoManifest> { CreateManifestWithContainer(5) }
                };

                var createResp = await client.PostAsJsonAsync("/api/VesselVisitNotifications", payload);
                createResp.StatusCode.Should().Be(HttpStatusCode.Created);
                var created = JsonConvert.DeserializeObject<dynamic>(await createResp.Content.ReadAsStringAsync());
                notifId = (Guid)created.id;
            }

            // Withdraw
            var withdrawResp = await client.PutAsync($"/api/VesselVisitNotifications/{notifId}/withdraw", null);
            withdrawResp.StatusCode.Should().Be(HttpStatusCode.OK);
            var wDto = JsonConvert.DeserializeObject<dynamic>(await withdrawResp.Content.ReadAsStringAsync());
            ((string)wDto.status).Should().Be("WithdrawnRequest");

            // Resume
            var resumeResp = await client.PutAsync($"/api/VesselVisitNotifications/{notifId}/resume", null);
            resumeResp.StatusCode.Should().Be(HttpStatusCode.OK);
            var rDto = JsonConvert.DeserializeObject<dynamic>(await resumeResp.Content.ReadAsStringAsync());
            ((string)rDto.status).Should().Be("InProgress");
        }

        [Fact]
        public async Task ApproveAfterCompleted_AllowsApproval()
        {
            var client = _factory.CreateClient();

            Guid notifId;

            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<DDDSample1DbContext>();

                var vt = new VesselType("TypeD","desc",2000,1,1,1);
                db.VesselTypes.Add(vt);
                var vessel = new Vessel("IMO1234570","V4", vt.Id, "owner", "operator");
                db.Vessels.Add(vessel);
                var rep = new Representative("Rep Name","rep-000","PT","rep4@example.com","444555666");
                db.Representatives.Add(rep);
                await db.SaveChangesAsync();

                var payload = new
                {
                    VesselId = vessel.Id.AsGuid(),
                    RepresentativeId = rep.Id.AsString(),
                    LoadingManifests = new List<CargoManifest> { CreateManifestWithContainer(10) },
                    UnloadingManifests = new List<CargoManifest> { CreateManifestWithContainer(5) }
                };

                var createResp = await client.PostAsJsonAsync("/api/VesselVisitNotifications", payload);
                createResp.StatusCode.Should().Be(HttpStatusCode.Created);
                var created = JsonConvert.DeserializeObject<dynamic>(await createResp.Content.ReadAsStringAsync());
                notifId = (Guid)created.id;

                // Set status to Completed via DbContext (reflection since setter is private)
                // load into memory and set status via reflection (Status has private setter)
                var all = await db.VesselVisitNotifications.ToListAsync();
                var notifEntity = all.Find(x => x.Id.AsGuid() == notifId);
                var statusProp = notifEntity.GetType().GetProperty("Status");
                statusProp.SetValue(notifEntity, NotificationStatus.Completed);
                await db.SaveChangesAsync();
            }

            // Approve
            var approveDto = new { DockId = "D1", OfficerId = "O1" };
            var approveResp = await client.PutAsJsonAsync($"/api/VesselVisitNotifications/{notifId}/approve", approveDto);
            approveResp.StatusCode.Should().Be(HttpStatusCode.OK);
            var aDto = JsonConvert.DeserializeObject<dynamic>(await approveResp.Content.ReadAsStringAsync());
            ((string)aDto.status).Should().Be("Approved");
            ((string)aDto.assignedDock).Should().Be("D1");
        }
    }
}
