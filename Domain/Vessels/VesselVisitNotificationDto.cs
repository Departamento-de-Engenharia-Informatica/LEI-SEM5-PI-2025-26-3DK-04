using System;

namespace DDDSample1.Domain.Vessels
{
    public class VesselVisitNotificationDto
    {
        public Guid Id { get; set; }
        public NotificationState State { get; set; }
        public Dock AssignedDock { get; set; }
        public string RejectedReason { get; set; }
        public DateTime? DecisionTimeStamp { get; set; }
        public string DecisionOutcome { get; set; }
        public string OfficerId { get; set; }
        
        public LoadingCargoMaterial LoadingCargo { get; set; }
        public UnloadingCargoMaterial UnloadingCargo { get; set; }

        public VesselVisitNotificationDto(LoadingCargoMaterial loadingCargo, UnloadingCargoMaterial unloadingCargo)
        {
            this.Id = new VesselVisitNotificationID(Guid.NewGuid());
            this.LoadingCargo = loadingCargo;
            this.UnloadingCargo = unloadingCargo;
        }        
    }
}
