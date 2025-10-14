using System.Collections.Generic;
using DDDSample1.Domain.Docks;

namespace DDDNetCore.Domain.PortInfrastructure.StorageArea;

public class StorageDockAssignment : ValueObject
{
    public DockId DockId { get; private set; }
    public double DistanceMeters { get; private set; }

    public StorageDockAssignment(DockId dockId, double distanceMeters)
    {
        DockId = dockId;
        DistanceMeters = distanceMeters;
    }

    protected override IEnumerable<object> GetEqualityComponents()
    {
        yield return DockId;
        yield return DistanceMeters;
    }
}
