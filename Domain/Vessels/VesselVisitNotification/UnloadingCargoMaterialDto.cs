using System.Collections.Generic;

namespace DDDSample1.Domain.Vessels.VesselInformation
{
    public class UnloadingCargoMaterialDTO
    {
        public List<CargoManifestDto> Manifests { get; set; } = new();
    }
}