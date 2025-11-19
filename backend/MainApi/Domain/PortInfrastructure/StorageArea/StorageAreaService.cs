using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Domain.PortInfrastructure.StorageArea;
using DDDNetCore.Domain.PortInfrastructure.StorageArea.DTOs;
// Using the correct DTO namespaces based on your provided files
using DDDNetCore.Infraestructure.PortInfrastructure.DTOs; // For StorageAreaDto, DockAssignmentDto
using DDDSample1.Domain.PortInfrastructure.StorageArea; // Assuming Create/Update/Assign DTOs are here
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Shared;
// Using the correct entity namespace
using DDDSample1.Domain.PortInfrastructure.StorageArea;
using DDDSample1.Domain.StorageAreas.DTOs;

// Correct namespace for the service
namespace DDDSample1.Domain.PortInfrastructure.StorageArea
{
    public class StorageAreaService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IStorageAreaRepository _repo;
        private readonly IDockRepository _dockRepo; // Inject Dock repository

        // --- Updated Constructor to inject dependencies ---
        public StorageAreaService(IUnitOfWork unitOfWork, IStorageAreaRepository repo, IDockRepository dockRepo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _dockRepo = dockRepo;
        }

        // --- Get All (Active) ---
        public async Task<List<StorageAreaDto>> GetAllAsync()
        {
            // Assuming GetAllAsync in repo includes assignments (due to override)
            var list = await _repo.GetAllAsync();
            list = list.Where(sa => sa.Active).ToList(); // Filter for active

            var dtoList = new List<StorageAreaDto>();
            foreach (var sa in list)
            {
                dtoList.Add(await ToDto(sa)); // Use await with async ToDto
            }
            return dtoList;
        }
        
        public async Task<List<StorageAreaDto>> GetAllYardsAsync()
        {
            // 1. Chamar o repositório para obter apenas os YARDS (StorageAreaType.Yard é 0)
            var yardAreas = await _repo.GetByStorageTypeAsync(StorageAreaType.Yard);
            
            var dtoList = new List<StorageAreaDto>();
            foreach (var sa in yardAreas)
            {
                dtoList.Add(await ToDto(sa));
            }

            return dtoList;
        }
        
        public async Task<List<StorageAreaDto>> GetAllWarehousesAsync()
        {
            // 1. Chamar o repositório para obter apenas os WAREHOUSES (StorageAreaType.Warehouse é 1)
            var warehouseAreas = await _repo.GetByStorageTypeAsync(StorageAreaType.Warehouse);
            var dtoList = new List<StorageAreaDto>();
            foreach (var sa in warehouseAreas)
            {
                dtoList.Add(await ToDto(sa));
            }

            return dtoList;;
        }

        // --- Get By ID (Active) ---
        public async Task<StorageAreaDto?> GetByIdAsync(StorageAreaID id)
        {
            // Assuming GetByIdAsync in repo includes assignments (due to override)
            var area = await _repo.GetByIdAsync(id);
            if (area == null || !area.Active)
            {
                return null; // Not found or inactive
            }
            return await ToDto(area); // Use await with async ToDto
        }
        

        // --- Add New Storage Area (using CreateStorageAreaDto) ---
        public async Task<StorageAreaDto> AddAsync(CreateStorageAreaDto dto) // Changed from RegisterAsync and uses Create DTO
        {
            // Optional: Check for duplicate code
            var existing = await _repo.GetByCodeAsync(dto.Code);
            if (existing != null && existing.Active)
            {
                throw new BusinessRuleValidationException($"An active storage area with code '{dto.Code}' already exists.");
            }

            var location = new Location(dto.Coordinates, dto.LocationDescription);

            // Use the StorageArea constructor matching your entity
            var area = new StorageArea(dto.Code, dto.Designation, dto.StorageAreaType, location, dto.MaxCapacityTEUs);

            // Handle initial dock assignments if provided
            if (dto.InitialDockAssignments != null)
            {
                foreach (var assignmentDto in dto.InitialDockAssignments)
                {
                    var dockId = new DockID(assignmentDto.DockId);
                    var dock = await _dockRepo.GetByIdAsync(dockId);
                    if (dock == null || !dock.Active)
                        throw new BusinessRuleValidationException($"Dock with ID {assignmentDto.DockId} not found or inactive.");
                    area.AssignDock(dockId, assignmentDto.DistanceMeters);
                }
            }

            await _repo.AddAsync(area); // Should return Task or Task<StorageArea> without saving
            await _unitOfWork.CommitAsync(); // Save changes

            return await ToDto(area); // Use await with async ToDto
        }

        // --- Update Existing Storage Area (using UpdateStorageAreaDto) ---
        public async Task<StorageAreaDto?> UpdateAsync(StorageAreaID id, UpdateStorageAreaDto dto) // Use Update DTO
        {
            var area = await _repo.GetByIdAsync(id); // Assumes includes assignments
            if (area == null || !area.Active)
            {
                return null; // Not found or inactive
            }

             // Optional: Check for duplicate code if code is being changed
            if (area.Code != dto.Code)
            {
                 var existing = await _repo.GetByCodeAsync(dto.Code);
                  if (existing != null && existing.Active && !existing.Id.Equals(area.Id))
                  {
                      throw new BusinessRuleValidationException($"An active storage area with code '{dto.Code}' already exists.");
                  }
            }

            var location = new Location(dto.Coordinates, dto.LocationDescription);

            // Call the entity's update method matching your entity
            area.UpdateDetails(dto.Code, dto.Designation, dto.StorageAreaType, location, dto.MaxCapacityTEUs, dto.CurrentOccupancyTEUs);

            // Repository UpdateAsync might not be needed if UoW tracks changes
            // await _repo.UpdateAsync(area); // Check if needed based on BaseRepository/UoW implementation
            await _unitOfWork.CommitAsync(); // Save changes

            return await ToDto(area); // Use await with async ToDto
        }

        // --- Assign a Dock ---
        public async Task<StorageAreaDto?> AssignDockAsync(StorageAreaID storageAreaId, AssignDockDto dto) // Use Assign DTO
        {
            var area = await _repo.GetByIdAsync(storageAreaId); // Assumes includes assignments
            if (area == null || !area.Active)
                throw new BusinessRuleValidationException($"Storage Area {storageAreaId.AsGuid()} not found or inactive.");

            var dockId = new DockID(dto.DockId);
            var dock = await _dockRepo.GetByIdAsync(dockId);
            if (dock == null || !dock.Active)
                throw new BusinessRuleValidationException($"Dock {dto.DockId} not found or inactive.");

            area.AssignDock(dockId, dto.DistanceMeters);

            await _unitOfWork.CommitAsync();
            return await ToDto(area);
        }

        // --- Unassign a Dock ---
        public async Task<StorageAreaDto?> UnassignDockAsync(StorageAreaID storageAreaId, DockID dockId)
        {
            var area = await _repo.GetByIdAsync(storageAreaId); // Assumes includes assignments
            if (area == null || !area.Active)
                throw new BusinessRuleValidationException($"Storage Area {storageAreaId.AsGuid()} not found or inactive.");

            // Optional: Check if dock exists before attempting unassign (though entity does it)
            // var dock = await _dockRepo.GetByIdAsync(dockId);
            // if (dock == null)
            //    throw new BusinessRuleValidationException($"Dock {dockId.AsGuid()} not found.");

            area.UnassignDock(dockId);

            await _unitOfWork.CommitAsync();
            return await ToDto(area);
        }

        // --- Soft Delete (Inactivate) ---
        public async Task<StorageAreaDto?> InactivateAsync(StorageAreaID id)
        {
            var area = await _repo.GetByIdAsync(id);
            if (area == null) return null; // Not found

            if (!area.Active) // Already inactive
                 throw new BusinessRuleValidationException($"Storage Area {id.AsGuid()} is already inactive.");

            area.MarkAsInactive();
            await _unitOfWork.CommitAsync();
            return await ToDto(area);
        }

         // --- Activate ---
        public async Task<StorageAreaDto?> ActivateAsync(StorageAreaID id)
        {
             var area = await _repo.GetByIdAsync(id);
             if (area == null) return null; // Not found

             if (area.Active) // Already active
                  throw new BusinessRuleValidationException($"Storage Area {id.AsGuid()} is already active.");

             area.MarkAsActive();
             await _unitOfWork.CommitAsync();
             return await ToDto(area);
        }

        // --- Updated Helper to convert Entity to DTO (now async) ---
        // Changed to private and made async
        private async Task<StorageAreaDto> ToDto(StorageArea area)
        {
            var dockAssignmentsDto = new List<AssignDockDto>();
            if (area.DockAssignments != null) // Check if assignments were loaded
            {
                foreach (var assignment in area.DockAssignments)
                {
                    // Fetch dock details asynchronously to get the name
                    var dock = await _dockRepo.GetByIdAsync(assignment.DockId);
                    dockAssignmentsDto.Add(new AssignDockDto
                    {
                        DockId = assignment.DockId.AsGuid(),
                        //DockName = dock?.Name ?? "Unknown/Inactive Dock", // Get dock name
                        DistanceMeters = assignment.DistanceMeters
                    });
                }
            }

            // Map ALL properties from the updated StorageArea entity to StorageAreaDto
            return new StorageAreaDto
            {
                Id = area.Id.AsGuid(),
                Code = area.Code,
                Designation = area.Designation,
                StorageAreaType = area.StorageAreaType, // Map Enum
                Coordinates = area.Location?.Coordinates, // Map from Location object
                LocationDescription = area.Location?.Description, // Map from Location object
                MaxCapacityTEUs = area.MaxCapacityTEUs,
                CurrentOccupancyTEUs = area.CurrentOccupancyTEUs,
                Active = area.Active,
                AssignedDocks = dockAssignmentsDto
            };
        }
    }
}