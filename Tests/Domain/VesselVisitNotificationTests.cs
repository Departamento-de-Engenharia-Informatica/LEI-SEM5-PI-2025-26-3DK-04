using System;
using System.Collections.Generic;
using System.Reflection;
using FluentAssertions;
using Xunit;
using DDDSample1.Domain.Vessels.VesselVisitNotification;
using DDDSample1.Domain.Vessels;
using DDDSample1.Domain.Vessels.VesselInformation;
using DDDSample1.Domain.Organizations;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Tests.Domain
{
    public class VesselVisitNotificationTests
    {
        private Vessel CreateValidVessel()
        {
            var vesselTypeId = new VesselTypeId(Guid.NewGuid());
            return new Vessel("IMO1234567", "Test Vessel", vesselTypeId, "Owner", "Operator");
        }

        private CargoManifest CreateManifestWithContainer(double weight)
        {
            // CargoManifest expects a GUID string as id
            var manifest = new CargoManifest(new List<Container>());

            // Generate a valid-ish container id (4 letters + 6 digits + check digit)
            string baseId = "ABCD" + new Random().Next(0, 999999).ToString("D6");
            int sum = 0;
            for (int i = 0; i < 10; i++)
            {
                char c = baseId[i];
                int charValue = char.IsLetter(c) ? (c - 'A' + 10) : (c - '0');
                sum += charValue * (int)Math.Pow(2, i);
            }
            int checkDigit = (sum % 11) % 10;
            string containerId = baseId + checkDigit.ToString();

            var container = new Container( weight, "contents");
            manifest.AddContainer(container);
            return manifest;
        }

        // Overload used by some tests that historically passed a string id; the domain requires a GUID id,
        // so ignore the supplied id and create a manifest with a valid GUID to avoid parsing errors.
        private CargoManifest CreateManifestWithContainer(string id, double weight)
        {
            return CreateManifestWithContainer(weight);
        }

        [Fact]
        public void WhenPassingCorrectData_ThenVesselVisitNotificationIsInstantiated()
        {
            var vessel = CreateValidVessel();
            var loading = new LoadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(1000) });
            var unloading = new UnloadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(500) });
            var repId = new RepresentativeId("13232");

            var notif = new VesselVisitNotification(vessel, loading, unloading, repId);

            notif.Vessel.Should().Be(vessel);
            notif.LoadingCargo.Should().NotBeNull();
            notif.UnloadingCargo.Should().NotBeNull();
            notif.RepresentativeId.Should().Be(repId);
            notif.Status.Should().Be(NotificationStatus.InProgress);
            notif.CreatedAt.Should().BeCloseTo(DateTime.UtcNow, TimeSpan.FromSeconds(5));
            notif.Id.Should().NotBeNull();
        }

        [Fact]
        public void WhenCreatingWithNullVessel_ThenThrows()
        {
            var loading = new LoadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(1000) });
            var unloading = new UnloadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(500) });
            var repId = new RepresentativeId("13232");

            Action act = () => new VesselVisitNotification(null, loading, unloading, repId);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Vessel is required for a visit notification.");
        }

        [Fact]
        public void WhenCreatingWithNullRepresentative_ThenThrows()
        {
            var vessel = CreateValidVessel();
            var loading = new LoadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(1000) });
            var unloading = new UnloadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(500) });

            Action act = () => new VesselVisitNotification(vessel, loading, unloading, null);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Representative is required for a visit notification.");
        }

        [Fact]
        public void UpdateInProgress_WithNullBothCargo_Throws()
        {
            var vessel = CreateValidVessel();
            var loading = new LoadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(1000) });
            var unloading = new UnloadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(500) });
            var repId = new RepresentativeId("13232");

            var notif = new VesselVisitNotification(vessel, loading, unloading, repId);

            Action act = () => notif.UpdateInProgress(null, null,null);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("At least one field (Vessel, LoadingCargo, or UnloadingCargo) must be provided for update.");
        }

        [Fact]
        public void UpdateInProgress_WhenNotInProgress_Throws()
        {
            var vessel = CreateValidVessel();
            var loading = new LoadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(1000) });
            var unloading = new UnloadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(500) });
            var repId = new RepresentativeId("13232");

            var notif = new VesselVisitNotification(vessel, loading, unloading, repId);

            // change status to Submitted using SubmitForApproval
            notif.SubmitForApproval();

            Action act = () => notif.UpdateInProgress(null,loading, null);

            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Only notifications in progress can be updated by a representative.");
        }

        [Fact]
        public void SubmitForApproval_OnlyFromInProgress()
        {
            var vessel = CreateValidVessel();
            var loading = new LoadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(1000) });
            var unloading = new UnloadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(500) });
            var repId = new RepresentativeId("13232");

            var notif = new VesselVisitNotification(vessel, loading, unloading, repId);

            notif.SubmitForApproval();
            notif.Status.Should().Be(NotificationStatus.Submitted);

            Action act = () => notif.SubmitForApproval();
            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Only notifications in progress can be submitted for approval.");
        }

        [Fact]
        public void Withdraw_OnlyFromInProgress()
        {
            var vessel = CreateValidVessel();
            var loading = new LoadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(1000) });
            var unloading = new UnloadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(500) });
            var repId = new RepresentativeId("13232");

            var notif = new VesselVisitNotification(vessel, loading, unloading, repId);

            notif.Withdraw();
            notif.Status.Should().Be(NotificationStatus.WithdrawnRequest);

            Action act = () => notif.Withdraw();
            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Only notifications in progress for approval can be withdrawn.");
        }

        [Fact]
        public void Resume_OnlyFromWithdrawn()
        {
            var vessel = CreateValidVessel();
            var loading = new LoadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(1000) });
            var unloading = new UnloadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer(500) });
            var repId = new RepresentativeId("13232");

            var notif = new VesselVisitNotification(vessel, loading, unloading, repId);

            notif.Withdraw();
            notif.Resume();
            notif.Status.Should().Be(NotificationStatus.InProgress);

            Action act = () => notif.Resume();
            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Only withdrawn notifications can be resumed.");
        }

        [Fact]
        public void Approve_And_Reject_RequireCompletedState_AndValidateParameters()
        {
            var vessel = CreateValidVessel();
            var loading = new LoadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer("m1", 1000) });
            var unloading = new UnloadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer("m2", 500) });
            var repId = new RepresentativeId("13232");

            var notif = new VesselVisitNotification(vessel, loading, unloading, repId);

            // Trying to approve when not Completed
            Action actApprove = () => notif.Approve("D1", "O1");
            actApprove.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Only notifications marked as completed can be approved.");

            // Trying to reject when not Completed
            Action actReject = () => notif.Reject("reason", "O1");
            actReject.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Only notifications marked as completed can be rejected.");

            // set status to Completed via reflection so we can test parameter validation and transitions
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(notif, NotificationStatus.Completed);

            // Approve with missing dock/officer should throw
            Action actApproveMissingDock = () => notif.Approve("", "O1");
            actApproveMissingDock.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("A dock must be assigned when approving a notification.");

            Action actApproveMissingOfficer = () => notif.Approve("D1", "");
            actApproveMissingOfficer.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Officer ID is required for approval.");

            // Now approve properly
            notif.Approve("D1", "O1");
            notif.Status.Should().Be(NotificationStatus.Approved);
            notif.AssignedDock.Should().Be("D1");
            notif.OfficerId.Should().Be("O1");
            notif.DecisionOutcome.Should().Be("Approved");

            // Reset to Completed and test Reject
            statusProp.SetValue(notif, NotificationStatus.Completed);

            Action actRejectMissingReason = () => notif.Reject("", "O2");
            actRejectMissingReason.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("A rejection reason must be provided.");

            Action actRejectMissingOfficer = () => notif.Reject("bad", "");
            actRejectMissingOfficer.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Officer ID is required for rejection.");

            notif.Reject("bad cargo", "O2");
            notif.Status.Should().Be(NotificationStatus.Rejected);
            notif.RejectedReason.Should().Be("bad cargo");
            notif.DecisionOutcome.Should().Be("Rejected");
        }

        [Fact]
        public void ResetToPending_OnlyFromRejected()
        {
            var vessel = CreateValidVessel();
            var loading = new LoadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer("m1", 1000) });
            var unloading = new UnloadingCargoMaterial(new List<CargoManifest>{ CreateManifestWithContainer("m2", 500) });
            var repId = new RepresentativeId("13232");

            var notif = new VesselVisitNotification(vessel, loading, unloading, repId);

            // set to Rejected
            var statusProp = typeof(VesselVisitNotification).GetProperty("Status", BindingFlags.Instance | BindingFlags.Public);
            statusProp.SetValue(notif, NotificationStatus.Rejected);

            notif.ResetToPending();
            notif.Status.Should().Be(NotificationStatus.Pending);

            Action act = () => notif.ResetToPending();
            act.Should().Throw<BusinessRuleValidationException>()
                .WithMessage("Only rejected notifications can be reset to pending.");
        }
    }
}
