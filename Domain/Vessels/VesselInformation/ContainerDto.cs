using System;

namespace DDDNetCore.Domain.Vessels.VesselInformation;

public class ContainerDTO
{
    public Guid Id { get; set; }
    public double PayloadWeight { get; set; }
    public string ContentsDescription { get; set; }
}