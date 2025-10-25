using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.Vessels.VesselInformation
{
    public class CargoManifestDto
    {
        public Guid Id { get; set; }
        public List<ContainerDTO> Containers { get; set; } = new List<ContainerDTO>();
    }
}