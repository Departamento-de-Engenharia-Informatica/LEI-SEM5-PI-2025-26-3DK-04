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
        public RepresentativeStatus Status { get; private set; }

        private Representative() { } // For EF Core

        public Representative(string name, string citizenId, string nationality, string email, string phoneNumber)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new BusinessRuleValidationException("Name is required.");
            if (string.IsNullOrWhiteSpace(citizenId))
                throw new BusinessRuleValidationException("Citizen ID is required.");
            Validators.ValidateCitizenId(citizenId);
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
            this.Status = RepresentativeStatus.Active; // default
        }

        public void AssignToOrganization(OrganizationId organizationId)
        {
            if (organizationId == null)
                throw new BusinessRuleValidationException("Organization ID cannot be null.");

            if (OrganizationId != null && OrganizationId != organizationId)
                throw new BusinessRuleValidationException("Representative already belongs to another organization.");

            this.OrganizationId = organizationId;
        }

        public void Update(
            string name,
            string citizenId,
            string nationality,
            string email,
            string phoneNumber
        )
        {
            if (!string.IsNullOrWhiteSpace(name))
                Name = name;
            if (!string.IsNullOrWhiteSpace(citizenId))
                CitizenId = citizenId;
            if (!string.IsNullOrWhiteSpace(nationality))
                Nationality = nationality;
            if (!string.IsNullOrWhiteSpace(email))
                Email = email;
            if (!string.IsNullOrWhiteSpace(phoneNumber))
                PhoneNumber = phoneNumber;
        }

        public void Deactivate()
        {
            if (this.Status == RepresentativeStatus.Inactive)
                throw new BusinessRuleValidationException("Representative is already inactive.");

            this.Status = RepresentativeStatus.Inactive;
        }

        public void Activate()
        {
            if (this.Status == RepresentativeStatus.Active)
                throw new BusinessRuleValidationException("Representative is already active.");

            this.Status = RepresentativeStatus.Active;
        }
    }
}
