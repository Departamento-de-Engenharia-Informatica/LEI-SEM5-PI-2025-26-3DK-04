using System;
using System.Collections.Generic;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Vessels.VesselInformation;

namespace DDDSample1.Domain.Vessels.VesselVisitNotification
{
    public class VesselVisitNotificationDto
    {
        public Guid Id { get; set; }
        public NotificationStatus Status { get; set; }
    // Assigned dock id (string). Stored as string in the entity so expose as string in DTO
    public string AssignedDock { get; set; }
        public string RejectedReason { get; set; }
        public DateTime? DecisionTimeStamp { get; set; }
        public string DecisionOutcome { get; set; }
        public string OfficerId { get; set; }

        public LoadingCargoMaterial LoadingCargo { get; set; }
        public UnloadingCargoMaterial UnloadingCargo { get; set; }
        
        public Guid VesselId { get; set; }
        public string VesselName { get; set; }
        public string VesselCallsign { get; set; }
        
        public string? RepresentativeId { get; set; }
        public DateTime CreatedAt { get; set; }
        
        // Novos campos para integração com IARTI
        public DateTime? ArrivalTime { get; set; }
        public DateTime? DepartureTime { get; set; }
        public int? UnloadTime { get; set; } // Em horas
        public int? LoadTime { get; set; } // Em horas
        
        public List<string> StaffMemberIds { get; set; }
        public List<string> PhysicalResourceIds { get; set; } // Crane IDs
        public string DockId { get; set; }

        public VesselVisitNotificationDto(LoadingCargoMaterial loadingCargo, UnloadingCargoMaterial unloadingCargo)
        {
            this.Id = Guid.NewGuid();
            this.LoadingCargo = loadingCargo;
            this.UnloadingCargo = unloadingCargo;
        }
    }
}

