using System.Collections.Generic;
using DDDSample1.Domain.Docks;

namespace DDDNetCore.Domain.PortInfrastructure.StorageArea;

public class StorageDockAssignment
{
    public DockID DockId { get; private set; }
    public double DistanceMeters { get; private set; }

    public StorageDockAssignment(DockID dockId, double distanceMeters)
    {
        DockId = dockId;
        DistanceMeters = distanceMeters;
    }

    protected IEnumerable<object> GetEqualityComponents()
    {
        yield return DockId;
        yield return DistanceMeters;
    }
}
