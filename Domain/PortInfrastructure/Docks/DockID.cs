using System;
using DDDSample1.Domain.Shared;
using Newtonsoft.Json;

namespace DDDSample1.Domain.Docks
{
    public class DockID : EntityId
    {
        [JsonConstructor]
        public DockID(Guid value) : base(value)
        {
        }

        public DockID(string value) : base(value)
        {
        }

        protected override object createFromString(string text)
        {
            return new Guid(text);
        }

        public override string AsString()
        {
            Guid obj = (Guid)base.ObjValue;
            return obj.ToString();
        }

        public Guid AsGuid()
        {
            return (Guid)base.ObjValue;
        }
    }
}