using System.Collections.Generic;
using DDDSample1.Domain.Vessels.VesselInformation;

namespace DDDSample1.Domain.Vessels.VesselVisitNotification;

public class LoadingCargoMaterialDto
{
        public List<VesselInformation.CargoManifestDto> Manifests { get; set; }
}