using System;
using Newtonsoft.Json;
using DDDSample1.Domain.Shared;


namespace DDDSample1.Domain.Organizations
{
    public class RepresentativeId : EntityId
    {
        [JsonConstructor]
        public RepresentativeId(Guid value) : base(value) { }

        public RepresentativeId(string value) : base(Guid.Parse(value)) { }

        public Guid AsGuid()
        {
            return (Guid)base.ObjValue;
        }

        protected override object createFromString(string text)
        {
            return new RepresentativeId(Guid.Parse(text));
        }

        public override string AsString()
        {
            return AsGuid().ToString();
        }
    }
}