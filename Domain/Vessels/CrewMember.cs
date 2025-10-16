using System.Collections.Generic;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels
{
    public class CrewMember : ValueObject
    {
        public string Name { get; private set; }
        public string CitizenId { get; private set; }
        public string Nationality { get; private set; }

        public CrewMember(string name, string citizenId, string nationality)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new BusinessRuleValidationException("Crew member name is required.");

            if (string.IsNullOrWhiteSpace(citizenId))
                throw new BusinessRuleValidationException("Citizen ID is required.");

            if (string.IsNullOrWhiteSpace(nationality))
                throw new BusinessRuleValidationException("Nationality is required.");

            Name = name;
            CitizenId = citizenId;
            Nationality = nationality;
        }

        protected override IEnumerable<object> GetEqualityComponents()
        {
            yield return Name;
            yield return CitizenId;
            yield return Nationality;
        }
    }
}