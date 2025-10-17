using System;

namespace DDDSample1.Domain.Organizations
{
    public class RepresentativeDto
    {
        public Guid Id { get; set; }
        public string Name { get; set; }
        public string CitizenId { get; set; }
        public string Nationality { get; set; }

        public string Email { get; set; }

        public string PhoneNumber { get; set; }
        public string OrganizationId { get; set; }
        public string Status { get; set; }
    }
}