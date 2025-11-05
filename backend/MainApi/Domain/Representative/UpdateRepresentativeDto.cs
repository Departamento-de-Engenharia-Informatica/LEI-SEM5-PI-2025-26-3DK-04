using System;

namespace DDDSample1.Domain.Organizations;
public class UpdateRepresentativeDto

{
    public string CitizenId { get; set; } // novo CitizenId
    public string Name { get; set; }
    public string Email { get; set; }
    public string PhoneNumber { get; set; }
    public string Nationality { get; set; }
}