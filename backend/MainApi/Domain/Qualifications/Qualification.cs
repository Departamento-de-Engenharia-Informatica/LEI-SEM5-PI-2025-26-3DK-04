using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Qualifications
{
    public class Qualification : Entity<QualificationID>, IAggregateRoot
    {
        public string Name { get; private set; }

        public Qualification(string name)
        {
            Validators.ValidateQualificationName(name);
            
            this.Id = new QualificationID(Guid.NewGuid());
            this.Name = name;
        }

        protected Qualification()
        {
            this.Name = "";
        }

        public void ChangeName(string name)
        {
            Validators.ValidateQualificationName(name);
            
            this.Name = name;
        }
    }
}