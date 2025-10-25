
using System;
using System.ComponentModel.DataAnnotations; // Keep for Range if desired

// Adjust namespace to where your DTOs reside
namespace DDDSample1.Domain.Vessels.VesselVisitNotification.DTOs
{
    public class ContainerInputDto
    {
        
        public Guid Id { get; set; }

        
        [Range(0.01, double.MaxValue, ErrorMessage = "Container payload weight must be positive.")]
        public double PayloadWeight { get; set; }

       
        public string ContentsDescription { get; set; }
    }
}