using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels.VesselInformation;

public class Container : Entity<ContainerID>
{
    public double PayloadWeight { get; }
    //public ContainerType Type { get; }
    public string ContentsDescription { get; private set; }

    private Container(ContainerID id, double payloadWeight, string contentsDescription)
    {
        if (payloadWeight <= 0)
            throw new BusinessRuleValidationException("Container payload weight must be positive");

        PayloadWeight = payloadWeight;
        ContentsDescription = contentsDescription;
        //Type = type;
    }
    public Container() { } // Parameterless constructor for EF Core

    public static Container Create(string identifier, double payloadWeight, string contentsDescription)
    {
        var id = new ContainerID(identifier);
        return new Container(id, payloadWeight, contentsDescription);
    }
}