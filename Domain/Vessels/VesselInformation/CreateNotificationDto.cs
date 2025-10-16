using System.Collections.Generic;
using DDDSample1.Domain.Vessels.VesselInformation;

namespace DDDSample1.Domain.Vessels;

public class CreateNotificationDto
{
    public List<CargoManifest> LoadingManifests { get; set; }
    public List<CargoManifest> UnloadingManifests { get; set; }
    public List<CrewMember> CrewMembers { get; set; }
}
