using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Organizations
{
    public class OrganizationId : EntityId
    {
        public OrganizationId(Guid value) : base(value) { }

        public OrganizationId(string value) : base(Guid.Parse(value)) { }

        public Guid AsGuid()
        {
            // Garante que sempre devolve Guid
            return (Guid) base.ObjValue;
        }

        protected override object createFromString(string text)
        {
            return new OrganizationId(Guid.Parse(text));
        }

        public override string AsString()
        {
            return AsGuid().ToString();
        }
        
    }
}