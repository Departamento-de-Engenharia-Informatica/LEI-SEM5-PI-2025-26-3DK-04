using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Organizations
{
    public class Representative : Entity<RepresentativeId>, IAggregateRoot
    {
        public string Name { get; private set; }
        public string CitizenId { get; private set; }
        public string Nationality { get; private set; }
        public string Email { get; private set; }
        public string PhoneNumber { get; private set; }
        public OrganizationId OrganizationId { get; private set; }

        private Representative() { }

        public Representative(string name, string citizenId, string nationality, string email, string phoneNumber)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new BusinessRuleValidationException("Representative name is required.");

            if (string.IsNullOrWhiteSpace(citizenId))
                throw new BusinessRuleValidationException("Citizen ID is required.");

            if (string.IsNullOrWhiteSpace(nationality))
                throw new BusinessRuleValidationException("Nationality is required.");

            if (string.IsNullOrWhiteSpace(email))
                throw new BusinessRuleValidationException("Email is required.");

            if (string.IsNullOrWhiteSpace(phoneNumber))
                throw new BusinessRuleValidationException("Phone number is required.");

            this.Id = new RepresentativeId(Guid.NewGuid());
            this.Name = name;
            this.CitizenId = citizenId;
            this.Nationality = nationality;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
        }

        public void AssignToOrganization(OrganizationId organizationId)
        {
            if (organizationId == null)
                throw new BusinessRuleValidationException("Organization ID cannot be null.");

            if (this.OrganizationId != null && this.OrganizationId != organizationId)
                throw new BusinessRuleValidationException("Representative already belongs to another organization.");

            this.OrganizationId = organizationId;
        }
    }
}
