using System;
using System.Collections.Generic;
using DDDSample1.Domain.Vessels.VesselInformation;

namespace DDDSample1.Domain.Vessels;

public class CargoManifestDto
{
    public Guid Id { get; set; }
    public List<ContainerDTO> Containers { get; set; } = new List<ContainerDTO>();
}