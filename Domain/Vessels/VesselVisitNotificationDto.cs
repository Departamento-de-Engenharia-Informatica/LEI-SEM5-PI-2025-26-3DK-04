using System;
using System.Collections.Generic;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Vessels.VesselInformation;

namespace DDDSample1.Domain.Vessels
{
    public class VesselVisitNotificationDto
    {
        public VesselVisitNotificationID Id { get; set; }
        public NotificationStatus Status { get; set; }
        public Dock AssignedDock { get; set; }
        public string RejectedReason { get; set; }
        public DateTime? DecisionTimeStamp { get; set; }
        public string DecisionOutcome { get; set; }
        public string OfficerId { get; set; }
        public List<CargoManifest> CargoManifests { get; set; }
        public List<CrewMemberDto> CrewMembers { get; set; }
        
        public LoadingCargoMaterial LoadingCargo { get; set; }
        public UnloadingCargoMaterial UnloadingCargo { get; set; }

        public VesselVisitNotificationDto(LoadingCargoMaterial loadingCargo, UnloadingCargoMaterial unloadingCargo)
        {
            this.Id = new VesselVisitNotificationID(Guid.NewGuid());
            this.LoadingCargo = loadingCargo;
            this.UnloadingCargo = unloadingCargo;
        }

        public VesselVisitNotificationDto()
        {
            throw new NotImplementedException();
        }
    }
}
