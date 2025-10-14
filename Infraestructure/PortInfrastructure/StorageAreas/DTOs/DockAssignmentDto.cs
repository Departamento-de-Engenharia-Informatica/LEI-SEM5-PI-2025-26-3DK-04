using System;

namespace DDDNetCore.Infraestructure.PortInfrastructure.DTOs;

public class DockAssignmentDto
{
    public Guid DockId { get; set; }
    public double DistanceMeters { get; set; }
}
