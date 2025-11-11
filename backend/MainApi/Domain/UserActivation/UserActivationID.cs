using System;
using DDDSample1.Domain.Shared;
using Newtonsoft.Json;

namespace DDDSample1.Domain.Authentication
{
    public class UserActivationID : EntityId
    {
        [JsonConstructor]
        public UserActivationID(string value) : base(Guid.Parse(value))
        {
        }

        public UserActivationID(Guid value) : base(value)
        {
        }

        protected override object createFromString(string value)
        {
            return Guid.Parse(value);
        }

        public override string AsString()
        {
            return ((Guid)base.ObjValue).ToString();
        }
    }
}