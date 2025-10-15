using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels.VesselInformation;

public class Container : IEquatable<Container>
{
    public string Identifier { get; }
    public double PayloadWeight { get; }
    //public ContainerType Type { get; }

    private Container(string identifier, double payloadWeight)
    {
        if (!IsValidIdentifier(identifier))
            throw new BusinessRuleValidationException("Container identifier must comply with ISO 6346:2022 standard");
        if (payloadWeight <= 0)
            throw new BusinessRuleValidationException("Container payload weight must be positive");

        Identifier = identifier;
        PayloadWeight = payloadWeight;
        //Type = type;
    }

    public static Container Create(string identifier, double payloadWeight)
    {
        return new Container(identifier, payloadWeight);
    }

    private static bool IsValidIdentifier(string identifier)
    {
        if (string.IsNullOrEmpty(identifier) || identifier.Length != 11)
            return false;

        // ISO 6346:2022 validation:
        // First 3 characters: Owner code (capital letters)
        // 4th character: Category identifier (U for freight containers)
        // Last 7 characters: Serial number (digits)
        return System.Text.RegularExpressions.Regex.IsMatch(identifier, @"^[A-Z]{3}U\d{7}$");
    }

    public bool Equals(Container? other)
    {
        if (other == null)
            return false;
        return Identifier == other.Identifier;
    }

    public override bool Equals(object? obj)
    {
        return Equals(obj as Container);
    }

    public override int GetHashCode()
    {
        return Identifier.GetHashCode();
    }
}