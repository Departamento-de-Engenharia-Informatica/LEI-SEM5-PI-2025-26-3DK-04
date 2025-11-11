using System;
using System.Collections.Generic;
using DDDSample1.Domain.Vessels.VesselInformation;

namespace DDDSample1.Domain.Vessels
{
    public class CreateNotificationDto
    {
        public Guid VesselId { get; set; }
        public string RepresentativeId { get; set; }
        public List<CargoManifestInputDto> LoadingManifests { get; set; }
        public List<CargoManifestInputDto> UnloadingManifests { get; set; }
        public List<CrewMemberInputDto> Crew { get; set; } // Optional
        
        // Novos campos para integração com IARTI
        public DateTime ArrivalTime { get; set; }
        public DateTime DepartureTime { get; set; }
        public List<string> StaffMemberIds { get; set; } // Optional
        public string PhysicalResourceId { get; set; } // Crane ID - Optional
        public string DockId { get; set; } // Optional
    }

    public class CargoManifestInputDto
    {
        public List<ContainerInputDto> Containers { get; set; }
    }

    public class ContainerInputDto
    {
        public double PayloadWeight { get; set; }
        public string ContentsDescription { get; set; }
    }

    public class CrewMemberInputDto
    {
        public string Name { get; set; }
        public string CitizenId { get; set; }
        public string Nationality { get; set; }
    }
}


