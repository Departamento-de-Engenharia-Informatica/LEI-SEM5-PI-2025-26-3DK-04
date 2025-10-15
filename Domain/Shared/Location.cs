namespace DDDSample1.Domain.Shared;

public class Location: IValueObject
{
    public string Coordinates { get; private set; }
    public string Description { get; private set; }

    public Location(string coordinates, string description)
    {
        if (string.IsNullOrWhiteSpace(coordinates)) throw new BusinessRuleValidationException("Coordinates are required.");
        Coordinates = coordinates;
        Description = description;
    }
}
