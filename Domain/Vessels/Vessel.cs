using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels
{
    public class Vessel : Entity<VesselId>, IAggregateRoot
    {
        public string ImoNumber { get; private set; }
        public string Name { get; private set; }
        public VesselTypeId VesselTypeId { get; private set; }
        public string Owner { get; private set; }
        public string Operator { get; private set; }
        public bool Active { get; private set; }
        public List<CrewMember> Crew { get; set; }

        // Required for EF Core
        private Vessel()
        {
            this.Active = true;
        }

        public Vessel(string imoNumber, string name, VesselTypeId vesselTypeId, string owner, string operatorName)
        {
            if (string.IsNullOrWhiteSpace(imoNumber))
                throw new BusinessRuleValidationException("IMO number is required.");

            if (!IsValidImoFormat(imoNumber))
                throw new BusinessRuleValidationException("IMO number must be in the format: IMO1234567 (7 digits after IMO prefix).");

            if (string.IsNullOrWhiteSpace(name))
                throw new BusinessRuleValidationException("Vessel name is required.");

            if (vesselTypeId == null)
                throw new BusinessRuleValidationException("Vessel type is required.");

            if (string.IsNullOrWhiteSpace(owner))
                throw new BusinessRuleValidationException("Vessel owner is required.");

            if (string.IsNullOrWhiteSpace(operatorName))
                throw new BusinessRuleValidationException("Vessel operator is required.");

            this.Id = new VesselId(Guid.NewGuid());
            this.ImoNumber = imoNumber;
            this.Name = name;
            this.VesselTypeId = vesselTypeId;
            this.Owner = owner;
            this.Operator = operatorName;
            this.Active = true;
            this.Crew = new List<CrewMember>();
        }

        public void ChangeName(string name)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the name of an inactive vessel.");

            if (string.IsNullOrWhiteSpace(name))
                throw new BusinessRuleValidationException("Vessel name is required.");

            this.Name = name;
        }

        public void ChangeVesselType(VesselTypeId vesselTypeId)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the type of an inactive vessel.");

            if (vesselTypeId == null)
                throw new BusinessRuleValidationException("Vessel type is required.");

            this.VesselTypeId = vesselTypeId;
        }

        public void ChangeOwner(string owner)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the owner of an inactive vessel.");

            if (string.IsNullOrWhiteSpace(owner))
                throw new BusinessRuleValidationException("Vessel owner is required.");

            this.Owner = owner;
        }

        public void ChangeOperator(string operatorName)
        {
            if (!this.Active)
                throw new BusinessRuleValidationException("It is not possible to change the operator of an inactive vessel.");

            if (string.IsNullOrWhiteSpace(operatorName))
                throw new BusinessRuleValidationException("Vessel operator is required.");

            this.Operator = operatorName;
        }

        public void MarkAsInactive()
        {
            this.Active = false;
        }

        public void MarkAsActive()
        {
            this.Active = true;
        }

        private static bool IsValidImoFormat(string imoNumber)
        {
            if (string.IsNullOrWhiteSpace(imoNumber))
                return false;

            var cleaned = imoNumber.Trim().ToUpper();
            
            if (!cleaned.StartsWith("IMO"))
                return false;

            var numericPart = cleaned.Substring(3).Trim();
            
            return numericPart.Length == 7 && long.TryParse(numericPart, out _);
        }
        public void setCrew(List<CrewMember> crewMembers)
        {
            this.Crew = crewMembers;
        }   
    }
}
