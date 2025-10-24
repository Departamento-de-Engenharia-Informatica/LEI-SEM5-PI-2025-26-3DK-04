using System.Text.RegularExpressions;

namespace DDDSample1.Domain.Shared;

public class Location: IValueObject
{
    public string Coordinates { get; private set; }
    public string Description { get; private set; }
// Regex for "latitude, longitude"
    private static readonly Regex CoordinateRegex = new Regex(
        @"^[-+]?([1-8]?\d(\.\d+)?|90(\.0+)?),\s*[-+]?(180(\.0+)?|((1[0-7]\d)|([1-9]?\d))(\.\d+)?)$");
    public Location(string coordinates, string description)
    {
        if (string.IsNullOrWhiteSpace(coordinates)) throw new BusinessRuleValidationException("Coordinates are required.");
        if (string.IsNullOrWhiteSpace(coordinates) || !CoordinateRegex.IsMatch(coordinates))
        {
            throw new BusinessRuleValidationException("Coordinates must be in a valid 'latitude, longitude' format.");
        }
        Coordinates = coordinates;
        Description = description;
    }
}
