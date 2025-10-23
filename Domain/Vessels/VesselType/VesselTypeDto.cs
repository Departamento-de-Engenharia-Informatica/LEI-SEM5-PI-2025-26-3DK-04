using System;

namespace DDDSample1.Domain.Vessels
{
    public class VesselTypeDto
    {
        public Guid Id { get; set; }
        public string Name { get; set; }
        public string Description { get; set; }
        public int Capacity { get; set; }
        public int MaxRows { get; set; }
        public int MaxBays { get; set; }
        public int MaxTiers { get; set; }
        public bool Active { get; set; }
    }
}
