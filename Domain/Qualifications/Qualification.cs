using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Qualifications
{
    /// <summary>
    /// Entidade Qualification representando uma qualificação
    /// </summary>
    public class Qualification : Entity<QualificationID>, IAggregateRoot
    {
        public string Name { get; private set; }
        public string Description { get; private set; }

        public Qualification(string name, string description)
        {
            this.Id = new QualificationID(Guid.NewGuid());
            this.Name = name;
            this.Description = description;
        }

        // Construtor vazio para EF
        protected Qualification()
        {
            this.Name = "";
            this.Description = "";
        }

        public void ChangeName(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new BusinessRuleValidationException("Name cannot be empty.");

            this.Name = name;
        }

        public void ChangeDescription(string description)
        {
            if (string.IsNullOrWhiteSpace(description))
                throw new BusinessRuleValidationException("Description cannot be empty.");

            this.Description = description;
        }
    }
}