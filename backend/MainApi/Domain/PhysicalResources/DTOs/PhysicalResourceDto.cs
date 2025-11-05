using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.PhysicalResources.DTOs;

public class PhysicalResourceDto
{
    public Guid Id { get; set; }
    public string Description { get; set; }
    public string Type { get; set; }
    public double Capacity { get; set; }
    public string? AssignedArea { get; set; }
    public int? SetupTime { get; set; }
    public ResourceStatus Status { get; set; }
    public List<string> QualificationIds { get; set; }
}

