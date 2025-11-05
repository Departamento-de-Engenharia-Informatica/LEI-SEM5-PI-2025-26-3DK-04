using System;
using System.Text.Json.Serialization;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.PhysicalResources;

public class PhysicalResourceId : EntityId
{
    [JsonConstructor]
    public PhysicalResourceId(Guid value) : base(value)
    {
    }

    public PhysicalResourceId(string value) : base(value)
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
