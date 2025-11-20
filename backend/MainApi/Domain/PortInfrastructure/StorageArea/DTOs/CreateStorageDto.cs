using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using DDDNetCore.Domain.PortInfrastructure.StorageArea;
using DDDNetCore.Infraestructure.PortInfrastructure.DTOs; // Keep for Range, EnumDataType, RegularExpression
using DDDSample1.Domain.PortInfrastructure.StorageArea;

namespace DDDSample1.Domain.StorageAreas.DTOs
{
    public class CreateStorageAreaDto
    {
        // Removed [Required], [StringLength]
        public string Code { get; set; }

        // Removed [Required], [StringLength]
        public string Designation { get; set; }

        // Keep validation for enum and format
        [EnumDataType(typeof(StorageAreaType), ErrorMessage = "Invalid storage area type value.")]
        public StorageAreaType StorageAreaType { get; set; }

        // Keep format validation
        [RegularExpression(@"^[-+]?([1-8]?\d(\.\d+)?|90(\.0+)?),\s*[-+]?(180(\.0+)?|((1[0-7]\d)|([1-9]?\d))(\.\d+)?)$",
            ErrorMessage = "Coordinates must be in a valid 'latitude, longitude' format (e.g., '41.1786, -8.6080').")]
        public string Coordinates { get; set; }

        // Removed [Required]
        public string LocationDescription { get; set; }

        // Keep range validation
        [Range(1, int.MaxValue, ErrorMessage = "Maximum capacity (TEUs) must be at least 1.")]
        public int MaxCapacityTEUs { get; set; }

        public List<AssignDockDto>? InitialDockAssignments { get; set; }
        
        public int Length { get; set; }
        public int Width { get; set; }
        public int Heigth { get; set; }
    }
}