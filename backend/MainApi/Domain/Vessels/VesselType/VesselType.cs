using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels
{
    public class VesselType : Entity<VesselTypeId>, IAggregateRoot
    {
        public string Name { get; private set; }
        public string Description { get; private set; }
        public int Capacity { get; private set; }
        public int MaxRows { get; private set; }
        public int MaxBays { get; private set; }
        public int MaxTiers { get; private set; }
        public bool Active { get; private set; }

        private VesselType()
        {
            this.Active = true;
        }

        public VesselType(string name, string description, int capacity, int maxRows, int maxBays, int maxTiers)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new BusinessRuleValidationException("Vessel type name is required.");
            
            if (string.IsNullOrWhiteSpace(description))
                throw new BusinessRuleValidationException("Vessel type description is required.");
            
            if (capacity <= 0)
                throw new BusinessRuleValidationException("Capacity must be greater than zero.");
            
            if (maxRows <= 0)
                throw new BusinessRuleValidationException("Maximum number of rows must be greater than zero.");
            
            if (maxBays <= 0)
                throw new BusinessRuleValidationException("Maximum number of bays must be greater than zero.");
            
            if (maxTiers <= 0)
                throw new BusinessRuleValidationException("Maximum number of tiers must be greater than zero.");

            this.Id = new VesselTypeId(Guid.NewGuid());
            this.Name = name;
            this.Description = description;
            this.Capacity = capacity;
            this.MaxRows = maxRows;
            this.MaxBays = maxBays;
            this.MaxTiers = maxTiers;
            this.Active = true;
        }

        public void ChangeName(string name)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the name of an inactive vessel type.");
            
            if (string.IsNullOrWhiteSpace(name))
                throw new BusinessRuleValidationException("Vessel type name is required.");
            
            this.Name = name;
        }

        public void ChangeDescription(string description)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the description of an inactive vessel type.");
            
            if (string.IsNullOrWhiteSpace(description))
                throw new BusinessRuleValidationException("Vessel type description is required.");
            
            this.Description = description;
        }

        public void ChangeCapacity(int capacity)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the capacity of an inactive vessel type.");
            
            if (capacity <= 0)
                throw new BusinessRuleValidationException("Capacity must be greater than zero.");
            
            this.Capacity = capacity;
        }

        public void ChangeOperationalConstraints(int maxRows, int maxBays, int maxTiers)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the operational constraints of an inactive vessel type.");
            
            if (maxRows <= 0)
                throw new BusinessRuleValidationException("Maximum number of rows must be greater than zero.");
            
            if (maxBays <= 0)
                throw new BusinessRuleValidationException("Maximum number of bays must be greater than zero.");
            
            if (maxTiers <= 0)
                throw new BusinessRuleValidationException("Maximum number of tiers must be greater than zero.");

            this.MaxRows = maxRows;
            this.MaxBays = maxBays;
            this.MaxTiers = maxTiers;
        }

        public void MarkAsInactive()
        {
            this.Active = false;
        }

        public void MarkAsActive()
        {
            this.Active = true;
        }
    }
}
