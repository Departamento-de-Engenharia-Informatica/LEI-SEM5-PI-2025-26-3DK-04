using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels.VesselInformation;

public class Container : Entity<ContainerID>
{
    
    public double PayloadWeight { get; }
    //public ContainerType Type { get; }
    public string ContentsDescription { get; private set; }

    public Container( double payloadWeight, string contentsDescription)
    {
        if (payloadWeight <= 0)
            throw new BusinessRuleValidationException("Container payload weight must be positive");
        this.Id = new ContainerID(Guid.NewGuid());
        PayloadWeight = payloadWeight;
        ContentsDescription = contentsDescription;
        //Type = type;
    }
    public Container() { } // Parameterless constructor for EF Core
    
}