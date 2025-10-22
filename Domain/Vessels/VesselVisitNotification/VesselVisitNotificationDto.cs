using System;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Vessels.VesselInformation;

namespace DDDSample1.Domain.Vessels.VesselVisitNotification
{
    public class VesselVisitNotificationDto
    {
        public Guid Id { get; set; }
        public NotificationStatus Status { get; set; }
        public Dock AssignedDock { get; set; }
        public string RejectedReason { get; set; }
        public DateTime? DecisionTimeStamp { get; set; }
        public string DecisionOutcome { get; set; }
        public string OfficerId { get; set; }

        public LoadingCargoMaterial LoadingCargo { get; set; }
        public UnloadingCargoMaterial UnloadingCargo { get; set; }
        
        public Guid VesselId { get; set; }
        public string VesselName { get; set; }
        public string VesselCallsign { get; set; }
        
        public Guid? RepresentativeId { get; set; }
        public DateTime CreatedAt { get; set; }

        public VesselVisitNotificationDto(LoadingCargoMaterial loadingCargo, UnloadingCargoMaterial unloadingCargo)
        {
            this.Id = Guid.NewGuid();
            this.LoadingCargo = loadingCargo;
            this.UnloadingCargo = unloadingCargo;
        }
    }
}
