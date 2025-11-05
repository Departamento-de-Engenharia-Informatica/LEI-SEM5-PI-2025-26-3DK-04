using System;
using System.Text.Json.Serialization;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels.VesselInformation;

public class CrewMemberID: EntityId

{
    [JsonConstructor]
    public CrewMemberID(Guid value) : base(value)
    {
    }

    public CrewMemberID(String value) : base(value)
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