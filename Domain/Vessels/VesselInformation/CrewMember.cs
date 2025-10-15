using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels.VesselInformation;

public class CrewMember : Entity<CrewMemberID>
{
    public string Name { get; private set; }
    public string CitizenId { get; private set; }
    public string Nationality { get; private set; }

    public CrewMember(string name, string citizenId, string nationality)
    {
        Name = name;
        CitizenId = citizenId;
        Nationality = nationality;
    }
}
