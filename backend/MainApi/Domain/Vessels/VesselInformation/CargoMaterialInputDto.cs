using System.Collections.Generic;

namespace DDDNetCore.Domain.Vessels.VesselInformation;

public class CargoMaterialInputDto
{
    public List<CargoManifestInputDto> Manifests { get; set; } = new List<CargoManifestInputDto>();
}