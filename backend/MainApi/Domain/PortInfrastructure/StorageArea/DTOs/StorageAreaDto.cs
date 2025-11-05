using System;
using System.Collections.Generic;

using System.Text.Json.Serialization;
using DDDNetCore.Domain.PortInfrastructure.StorageArea;
using DDDNetCore.Infraestructure.PortInfrastructure.DTOs;

namespace DDDNetCore.Domain.PortInfrastructure.StorageArea.DTOs
{
    public class StorageAreaDto
    {
        public Guid Id { get; set; }

        // Added properties based on StorageArea entity
        public string Code { get; set; }
        public string Designation { get; set; }

        // Use the enum
        [JsonConverter(typeof(JsonStringEnumConverter))] // Return "Yard", "Warehouse" instead of 0, 1
        public StorageAreaType StorageAreaType { get; set; }

        // Properties from Location value object
        public string Coordinates { get; set; }
        public string LocationDescription { get; set; }

        // Existing properties
        public int MaxCapacityTEUs { get; set; }
        public int CurrentOccupancyTEUs { get; set; }

        // Added property
        public bool Active { get; set; }

        // Keep AssignedDocks (ensure DockAssignmentDto includes DockName if needed)
        public List<AssignDockDto> AssignedDocks { get; set; } = new List<AssignDockDto>();
    }
}