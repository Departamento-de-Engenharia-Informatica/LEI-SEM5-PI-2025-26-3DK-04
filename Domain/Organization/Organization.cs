using System;
using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Organizations
{
    public class Organization : Entity<OrganizationId>, IAggregateRoot
    {
        public string LegalName { get; private set; }
        public string AlternativeName { get; private set; }
        public string Address { get; private set; }
        public string TaxNumber { get; private set; }
        private readonly List<Representative> _representatives = new();

        public IReadOnlyCollection<Representative> Representatives => _representatives.AsReadOnly();

        private Organization() { } // EF Core

        public Organization(string legalName, string alternativeName, string address, string taxNumber)
        {
            if (string.IsNullOrWhiteSpace(legalName))
                throw new BusinessRuleValidationException("Legal name is required.");

            if (string.IsNullOrWhiteSpace(address))
                throw new BusinessRuleValidationException("Address is required.");

            if (string.IsNullOrWhiteSpace(taxNumber))
                throw new BusinessRuleValidationException("Tax number is required.");

            this.Id = new OrganizationId(Guid.NewGuid());
            this.LegalName = legalName;
            this.AlternativeName = alternativeName;
            this.Address = address;
            this.TaxNumber = taxNumber;
        }

        public void AddRepresentative(Representative rep)
        {
            if (rep == null)
                throw new BusinessRuleValidationException("Representative cannot be null.");

            if (rep.OrganizationId != null && rep.OrganizationId != this.Id)
                throw new BusinessRuleValidationException("Representative already assigned to another organization.");

            if (_representatives.Any(r => r.Email == rep.Email))
                throw new BusinessRuleValidationException("A representative with this email already exists in this organization.");

            rep.AssignToOrganization(this.Id);
            _representatives.Add(rep);
        }

        public bool HasRepresentative()
        {
            return _representatives != null && _representatives.Any();
        }
        
        public void ValidateReadyForRegistration()
        {
            if (!HasRepresentative())
                throw new BusinessRuleValidationException("An organization must have at least one representative at registration.");
        }
    }
}
