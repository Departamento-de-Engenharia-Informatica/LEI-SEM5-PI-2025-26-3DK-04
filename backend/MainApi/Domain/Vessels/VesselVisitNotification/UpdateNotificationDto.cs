
using System;
using System.Collections.Generic;
using DDDSample1.Domain.Vessels.VesselInformation;
using DDDSample1.Domain.Vessels; // Para incluir CrewMemberInputDto

namespace DDDSample1.Domain.Vessels.VesselVisitNotification
{
    public class UpdateNotificationDto
    {
        public string? VesselId { get; set; } 
        
        // Carga
        public LoadingCargoMaterialDto? LoadingCargo { get; set; }
        public UnloadingCargoMaterialDTO? UnloadingCargo { get; set; }
        
        // IARTI/Tempos
        public DateTime? ArrivalTime { get; set; }
        public DateTime? DepartureTime { get; set; }
        public List<string>? StaffMemberIds { get; set; }
        public List<string>? PhysicalResourceIds { get; set; } // Crane IDs
        public string? DockId { get; set; } // Dock ID para IARTI
        
        // Informação do Vessel (Crew)
        public List<CrewMemberInputDto>? Crew { get; set; }
    }
}