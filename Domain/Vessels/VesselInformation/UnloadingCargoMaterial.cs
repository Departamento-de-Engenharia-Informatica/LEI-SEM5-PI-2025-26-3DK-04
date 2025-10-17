using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels.VesselInformation;

public class UnloadingCargoMaterial : ValueObject
{
    private readonly List<CargoManifest> _manifests = new();
    public IReadOnlyCollection<CargoManifest> Manifests => _manifests.AsReadOnly();

    // Parameterless constructor for EF Core
    private UnloadingCargoMaterial()
    {
    }

    public UnloadingCargoMaterial(IEnumerable<CargoManifest> manifests)
    {
        if (manifests == null || !manifests.Any())
            throw new BusinessRuleValidationException("Unloading cargo must include at least one manifest.");

        _manifests.AddRange(manifests);
    }

    public double TotalWeightKg()
    {
        return _manifests.Sum(m => m.TotalWeightKg());
    }

    protected override IEnumerable<object> GetEqualityComponents()
    {
        foreach (var manifest in _manifests)
            yield return manifest;
    }
}
