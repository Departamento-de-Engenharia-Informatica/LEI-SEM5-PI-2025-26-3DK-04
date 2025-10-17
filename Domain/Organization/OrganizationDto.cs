using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.Organizations
{
    public class OrganizationDto
    {
        public string Id { get; set; }
        public string LegalName { get; set; }
        public string AlternativeName { get; set; }
        public string Address { get; set; }
        public string TaxNumber { get; set; }
        public List<RepresentativeDto> Representatives { get; set; }
    }
}