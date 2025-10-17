using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Vessels;
using System.Linq;
using DDDSample1.Domain.Vessels;


namespace DDDSample1.Domain.Docks;

public class Dock : Entity<DockID>, IAggregateRoot
{
    public string Name { get; private set; }
    public double Length { get; private set; }
    public double Depth { get; private set; }
    public int MaxDraft { get; private set; }
    public Location Location { get; private set; }
    public bool Active { get; private set; } = true;


    private readonly List<VesselType> _allowedVesselTypes = new();
    public IReadOnlyCollection<VesselType> AllowedVesselTypes => _allowedVesselTypes.AsReadOnly();
        
    // Construtor usado pelo EF Core
    private Dock() { }
    
    public Dock(string name, double length, double depth, int maxDraft, Location location, List<VesselType> vesselTypes)
    {
        if (string.IsNullOrWhiteSpace(name)) throw new BusinessRuleValidationException("Dock name is required.");
        if (vesselTypes == null || !vesselTypes.Any()) throw new BusinessRuleValidationException("At least one vessel type must be assigned.");

        Id = new DockID(Guid.NewGuid());
        Name = name;
        Length = length;
        Depth = depth;
        MaxDraft = maxDraft;
        Location = location;
        _allowedVesselTypes = vesselTypes;
    }

    public void Update(string name, double length, double depth, int maxDraft, Location location, List<VesselType> vesselTypes)
    {
        Name = name;
        Length = length;
        Depth = depth;
        MaxDraft = maxDraft;
        Location = location;
        _allowedVesselTypes.Clear();
        _allowedVesselTypes.AddRange(vesselTypes);
    }
    public void MarkAsInactive()
    {
        if (!Active)
            throw new BusinessRuleValidationException("Dock is already inactive.");

        Active = false;
    }
    public bool IsActive()
    {
        return Active;
    }


}

