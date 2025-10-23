using System;

namespace DDDSample1.Domain.Vessels
{
    public class VesselDto
    {
        public Guid Id { get; set; }
        public string ImoNumber { get; set; }
        public string Name { get; set; }
        public Guid VesselTypeId { get; set; }
        public string VesselTypeName { get; set; }
        public string Owner { get; set; }
        public string Operator { get; set; }
        public bool Active { get; set; }
    }
}
