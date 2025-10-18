using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels.VesselInformation;

public class CargoManifest : Entity<CargoManifestID>
{
    private List<Container> _containers;
    //private double _weight;

    private CargoManifest(CargoManifestID id)
    {
        _containers = new List<Container>();
    }
    public CargoManifest() { } // Parameterless constructor for EF Core

    public static CargoManifest Create(string id)
    {
        return new CargoManifest(new CargoManifestID(id));
    }

    public void AddContainer(Container container)
    {
        _containers.Add(container);
    }

    public IReadOnlyCollection<Container> Containers => _containers.AsReadOnly();
    public double TotalWeightKg()
    {
        return _containers.Sum(c => c.PayloadWeight);
    }

}