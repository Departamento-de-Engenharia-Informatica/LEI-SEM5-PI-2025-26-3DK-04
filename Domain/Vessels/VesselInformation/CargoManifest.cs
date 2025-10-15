using System.Collections.Generic;
using System.Text.RegularExpressions;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels.VesselInformation;

public class CargoManifest : Entity<CargoManifestID>
{
    private List<Container> _containers;
    
    public void AddContainer(Container container)
    {
        if (!IsValidContainerIdentifier(container.Id))
            throw new BusinessRuleValidationException("Invalid container identifier format");
        _containers.Add(container);
    }

    private bool IsValidContainerIdentifier(string identifier)
    {
        // ISO 6346:2022 validation logic
        return Regex.IsMatch(identifier, @"^[A-Z]{3}U\d{7}$");
    }
}
