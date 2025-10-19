using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.PhysicalResources.DTOs;

public class UpdatePhysicalResourceDto
{
    public string Description { get; set; }
    public double Capacity { get; set; }
    public string? AssignedArea { get; set; }
    public int? SetupTime { get; set; }
    public List<Guid> QualificationIds { get; set; }
}

