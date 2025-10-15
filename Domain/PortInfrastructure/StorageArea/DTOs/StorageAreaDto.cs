using System;
using System.Collections.Generic;

namespace DDDNetCore.Infraestructure.PortInfrastructure.DTOs;

public class StorageAreaDto
{
    public Guid Id { get; set; }
    public string Type { get; set; }
    public string Location { get; set; }
    public int MaxCapacityTEUs { get; set; }
    public int CurrentOccupancyTEUs { get; set; }
    public List<DockAssignmentDto> AssignedDocks { get; set; }
}
