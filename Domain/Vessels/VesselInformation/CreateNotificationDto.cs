using System;
using System.Collections.Generic;
using DDDSample1.Domain.Vessels.VesselInformation;

namespace DDDSample1.Domain.Vessels
{
    public class CreateNotificationDto
    {
        public Guid VesselId { get; set; }
        public Guid RepresentativeId { get; set; }
        public List<CargoManifest> LoadingManifests { get; set; }
        public List<CargoManifest> UnloadingManifests { get; set; }
        public List<CrewMemberDto> Crew { get; set; } // Optional
    }

    
}
