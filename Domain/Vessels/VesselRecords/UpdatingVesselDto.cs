using System;

namespace DDDSample1.Domain.Vessels
{
    public class UpdatingVesselDto
    {
        public string Name { get; set; }
        public Guid VesselTypeId { get; set; }
        public string Owner { get; set; }
        public string Operator { get; set; }
    }
}
