using System;
using System.Collections.Generic;
using DDDSample1.Domain.Qualifications;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.PhysicalResources;

public class PhysicalResource : Entity<PhysicalResourceId>, IAggregateRoot
{
    public string Description { get; private set; }
    public string Type { get; private set; }
    public double Capacity { get; private set; }
    public string? AssignedArea { get; private set; }
    public int? SetupTime { get; private set; }
    public ResourceStatus Status { get; private set; }

    private readonly List<QualificationID> _qualificationIds = new();
    public IReadOnlyCollection<QualificationID> QualificationIds => _qualificationIds.AsReadOnly();

    private PhysicalResource() { }

    public PhysicalResource(string description, string type, double capacity, string? assignedArea, int? setupTime, ResourceStatus status, List<QualificationID> qualificationIds)
    {
        if (string.IsNullOrWhiteSpace(description)) throw new BusinessRuleValidationException("Description is required.");
        if (string.IsNullOrWhiteSpace(type)) throw new BusinessRuleValidationException("Type is required.");
        if (capacity <= 0) throw new BusinessRuleValidationException("Capacity must be positive.");

        Id = new PhysicalResourceId(Guid.NewGuid());
        Description = description;
        Type = type;
        Capacity = capacity;
        AssignedArea = assignedArea;
        SetupTime = setupTime;
        Status = status;
        _qualificationIds = qualificationIds ?? new List<QualificationID>();
    }

    public void Update(string description, double capacity, string? assignedArea, int? setupTime, List<QualificationID> qualificationIds)
    {
        Description = description;
        Capacity = capacity;
        AssignedArea = assignedArea;
        SetupTime = setupTime;
        _qualificationIds.Clear();
        _qualificationIds.AddRange(qualificationIds);
    }

    public void ChangeStatus(ResourceStatus newStatus)
    {
        Status = newStatus;
    }
}

