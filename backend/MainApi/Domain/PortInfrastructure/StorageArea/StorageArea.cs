using System;
using System.Collections.Generic;
using System.Linq;
using DDDNetCore.Domain.PortInfrastructure.StorageArea;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.PortInfrastructure.StorageArea
{
    public class StorageArea : Entity<StorageAreaID>, IAggregateRoot
    {
        public string Code { get; private set; }
        public string Designation { get; private set; }
        public StorageAreaType StorageAreaType { get; private set; } // Changed from class/ID
        public Location Location { get; private set; }
        public int MaxCapacityTEUs { get; private set; }
        public int CurrentOccupancyTEUs { get; private set; }
        public bool Active { get; private set; }

        private readonly List<StorageDockAssignment> _dockAssignments = new();
        public IReadOnlyCollection<StorageDockAssignment> DockAssignments => _dockAssignments.AsReadOnly();
        
        public int Length { get; private set; }
        public int Width { get; private set; }
        public int Heigth { get; private set; }
        
        
        private StorageArea()
        {
            this.Active = true;
        }

        
        public StorageArea(string code, string designation, StorageAreaType storageAreaType, Location location, int maxCapacityTEUs, int length, int width, int heigth)
        {
            this.Id = new StorageAreaID(Guid.NewGuid());

            
            if (string.IsNullOrWhiteSpace(code))
                throw new BusinessRuleValidationException("Storage area code cannot be empty.");
            if (string.IsNullOrWhiteSpace(designation))
                throw new BusinessRuleValidationException("Storage area designation cannot be empty.");
            // Enum validation (optional, ensures it's a defined value, though type system helps)
            if (!Enum.IsDefined(typeof(StorageAreaType), storageAreaType))
                 throw new BusinessRuleValidationException("Invalid storage area type provided.");
            if (location == null)
                throw new BusinessRuleValidationException("Storage area location must be provided.");
            if (maxCapacityTEUs <= 0)
                 throw new BusinessRuleValidationException("Maximum capacity must be greater than 0.");

            
            this.Code = code;
            this.Designation = designation;
            this.StorageAreaType = storageAreaType; 
            this.Location = location;
            this.MaxCapacityTEUs = maxCapacityTEUs;
            this.CurrentOccupancyTEUs = 0;
            this.Active = true;
            this.Length = length;
            this.Width = width;
            this.Heigth = heigth;
        }

        
        public void UpdateDetails(string code, string designation, StorageAreaType storageAreaType, Location location, int maxCapacityTEUs, int currentOccupancyTEUs, int length, int depth, int heigth)
        {
            
            if (string.IsNullOrWhiteSpace(code))
                throw new BusinessRuleValidationException("Storage area code cannot be empty.");
            if (string.IsNullOrWhiteSpace(designation))
                throw new BusinessRuleValidationException("Storage area designation cannot be empty.");
            if (!Enum.IsDefined(typeof(StorageAreaType), storageAreaType))
                 throw new BusinessRuleValidationException("Invalid storage area type provided.");
            if (location == null)
                throw new BusinessRuleValidationException("Storage area location must be provided.");
            if (maxCapacityTEUs <= 0)
                 throw new BusinessRuleValidationException("Maximum capacity must be greater than 0.");
            if (currentOccupancyTEUs < 0)
                 throw new BusinessRuleValidationException("Current occupancy cannot be negative.");
            if (currentOccupancyTEUs > maxCapacityTEUs)
                throw new BusinessRuleValidationException("Current occupancy cannot exceed maximum capacity.");

            
            this.Code = code;
            this.Designation = designation;
            this.StorageAreaType = storageAreaType; // Assign enum value
            this.Location = location;
            this.MaxCapacityTEUs = maxCapacityTEUs;
            this.CurrentOccupancyTEUs = currentOccupancyTEUs;
            this.Length = length;
            this.Width = depth;
            this.Heigth = heigth;
        }

        
        public void AssignDock(DockID dockId, double distanceMeters)
        {
            if (dockId == null)
                 throw new BusinessRuleValidationException("Dock ID must be provided for assignment.");
            if (distanceMeters <= 0)
                 throw new BusinessRuleValidationException("Distance must be greater than 0.");
            if (_dockAssignments.Any(a => a.DockId.Equals(dockId)))
                throw new BusinessRuleValidationException($"Dock {dockId.AsString()} already assigned to this storage area.");

            _dockAssignments.Add(new StorageDockAssignment(dockId, distanceMeters));
        }

        public void MarkAsInactive()
        {
            this.Active = false;
        }

         public void MarkAsActive()
        {
            this.Active = true;
        }

        public void UnassignDock(DockID dockId)
        {
             if (dockId == null)
                 throw new BusinessRuleValidationException("Dock ID must be provided for unassignment.");
             var assignment = _dockAssignments.FirstOrDefault(a => a.DockId.Equals(dockId));
            if (assignment == null)
                 throw new BusinessRuleValidationException($"Dock {dockId.AsString()} is not assigned to this storage area.");

            _dockAssignments.Remove(assignment);
        }
    }
}