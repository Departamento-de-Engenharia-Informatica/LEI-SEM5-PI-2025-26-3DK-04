using System;
using System.Collections.Generic;
using System.Linq;
using DDDNetCore.Domain.PortInfrastructure.StorageArea;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.PortInfrastructure.StorageArea;

public class StorageArea : Entity<StorageAreaID>
{
    public string Type { get; private set; } // e.g., "yard", "warehouse"
    public string Location { get; private set; } // e.g., "North Sector"
    public int MaxCapacityTEUs { get; private set; }
    public int CurrentOccupancyTEUs { get; private set; }

    private readonly List<StorageDockAssignment> _dockAssignments = new();
    public IReadOnlyCollection<StorageDockAssignment> DockAssignments => _dockAssignments.AsReadOnly();

    public StorageArea(string type, string location, int maxCapacityTEUs)
    {
        Id = new StorageAreaID(Guid.NewGuid());
        Type = type;
        Location = location;
        MaxCapacityTEUs = maxCapacityTEUs;
        CurrentOccupancyTEUs = 0;
    }

    public void UpdateDetails(string type, string location, int maxCapacityTEUs, int currentOccupancyTEUs)
    {
        if (currentOccupancyTEUs > maxCapacityTEUs)
            throw new BusinessRuleValidationException("Current occupancy cannot exceed maximum capacity.");

        Type = type;
        Location = location;
        MaxCapacityTEUs = maxCapacityTEUs;
        CurrentOccupancyTEUs = currentOccupancyTEUs;
    }

    public void AssignDock(DockId dockId, double distanceMeters)
    {
        if (_dockAssignments.Any(a => a.DockId.Equals(dockId)))
            throw new BusinessRuleValidationException("Dock already assigned.");

        _dockAssignments.Add(new StorageDockAssignment(dockId, distanceMeters));
    }
}
