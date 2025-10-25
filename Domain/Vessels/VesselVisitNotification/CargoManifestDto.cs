using System.Collections.Generic;

namespace DDDSample1.Domain.Vessels.VesselInformation
{
    public class CargoManifestDTO
    {
        public string Id { get; set; }
        public List<ContainerDTO> Containers { get; set; }
    }
}