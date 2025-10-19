using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.PhysicalResources.DTOs;

public class CreatePhysicalResourceDto
{
    public string Description { get; set; }
    public string Type { get; set; }
    public double Capacity { get; set; }
    public string? AssignedArea { get; set; }
    public int? SetupTime { get; set; }
    public ResourceStatus Status { get; set; }
    public List<Guid> QualificationIds { get; set; }
}

