using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels.VesselInformation
{
    public class Container : Entity<ContainerID>
    {
        public double PayloadWeight { get; private set; }
        public string ContentsDescription { get; private set; }

        private Container() { } // Requerido por EF Core

        public Container(double payloadWeight, string contentsDescription)
        {
            if (payloadWeight <= 0)
                throw new BusinessRuleValidationException("Container payload weight must be positive.");

            Id = new ContainerID(Guid.NewGuid());
            PayloadWeight = payloadWeight;
            ContentsDescription = contentsDescription ?? throw new BusinessRuleValidationException("Contents description cannot be null.");
        }
    }
}