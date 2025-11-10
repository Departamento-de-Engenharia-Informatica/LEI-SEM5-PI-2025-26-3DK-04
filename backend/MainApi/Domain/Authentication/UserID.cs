using System;
using DDDSample1.Domain.Shared;
using Newtonsoft.Json;

namespace DDDSample1.Domain.Authentication
{
    public class UserID : EntityId
    {
        [JsonConstructor]
        public UserID(string value) : base(value)
        {
        }
        protected override object createFromString(string value)
        {
            return value;
        }

        public override string AsString()
        {
            return (string)base.ObjValue;
        }
    }
}