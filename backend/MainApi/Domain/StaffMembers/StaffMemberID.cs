using System;
using DDDSample1.Domain.Shared;
using Newtonsoft.Json;

namespace DDDSample1.Domain.StaffMembers
{
    public class StaffMemberID : EntityId
    {
        [JsonConstructor]
        public StaffMemberID(Guid value) : base(value)
        {
        }

        public StaffMemberID(String value) : base(value)
        {
        }

        override
        protected Object createFromString(String text){
            return new Guid(text);
        }
        
        override
        public String AsString(){
            Guid obj = (Guid) base.ObjValue;
            return obj.ToString();
        }
        public Guid AsGuid(){
            return (Guid) base.ObjValue;
        }
    }
}
