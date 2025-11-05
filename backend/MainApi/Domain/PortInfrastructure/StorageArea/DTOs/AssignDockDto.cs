using System;

namespace DDDNetCore.Infraestructure.PortInfrastructure.DTOs // Use your DTO namespace
{
    public class AssignDockDto
    {
        public Guid DockId { get; set; }
        public double DistanceMeters { get; set; }
    }
}