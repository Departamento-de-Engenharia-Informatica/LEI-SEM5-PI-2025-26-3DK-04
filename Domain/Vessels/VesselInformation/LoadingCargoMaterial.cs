using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels.VesselInformation;

public class LoadingCargoMaterial : ValueObject
{
    private readonly List<CargoManifest> _manifests = new();
    public IReadOnlyCollection<CargoManifest> Manifests => _manifests.AsReadOnly();

    public LoadingCargoMaterial(IEnumerable<CargoManifest> manifests)
    {
        if (manifests == null || !manifests.Any())
            throw new BusinessRuleValidationException("Loading cargo must include at least one manifest.");

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
