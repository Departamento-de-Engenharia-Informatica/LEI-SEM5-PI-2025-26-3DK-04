using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Infraestructure.Docks;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Vessels;

namespace DDDSample1.Infrastructure.Docks
{
    public class DockService
    {
        private readonly IDockRepository _dockRepo;
        private readonly IVesselTypeRepository _vesselTypeRepo;

        public DockService(IDockRepository dockRepo, IVesselTypeRepository vesselTypeRepo)
        {
            _dockRepo = dockRepo;
            _vesselTypeRepo = vesselTypeRepo;
        }

        public async Task<DockDetailsDto> AddAsync(DockDto dto)
        {
            var vesselTypes = await GetVesselTypes(dto.VesselTypeIds);

            var location = new Location(dto.Coordinates, dto.LocationDescription);
            var dock = new Dock(dto.Name, dto.Length, dto.Depth, dto.MaxDraft, location, vesselTypes);

            await _dockRepo.AddAsync(dock);

            return ToDetailsDto(dock);
        }

        public async Task<DockDetailsDto> UpdateAsync(string id, DockDto dto)
        {
            var dockId = new DockID(id);
            var dock = await _dockRepo.GetByIdAsync(dockId);
            if (dock == null)
                return null;

            var vesselTypes = await GetVesselTypes(dto.VesselTypeIds);
            var location = new Location(dto.Coordinates, dto.LocationDescription);

            dock.Update(dto.Name, dto.Length, dto.Depth, dto.MaxDraft, location, vesselTypes);
            await _dockRepo.UpdateAsync(dock);

            return ToDetailsDto(dock);
        }

        public async Task<DockDetailsDto> GetByIdAsync(DockID id)
        {
            var dock = await _dockRepo.GetByIdAsync(id);
            return dock == null ? null : ToDetailsDto(dock);
        }

        public async Task<List<DockDetailsDto>> GetAllAsync()
        {
            var docks = await _dockRepo.GetAllAsync();
            return docks.Select(ToDetailsDto).ToList();
        }

        public async Task<DockDetailsDto> InactivateAsync(DockID id)
        {
            var dock = await _dockRepo.GetByIdAsync(id);
            if (dock == null) return null;

            dock.MarkAsInactive();
            await _dockRepo.UpdateAsync(dock);

            return ToDetailsDto(dock);
        }

        public async Task<DockDetailsDto> DeleteAsync(DockID id)
        {
            var dock = await _dockRepo.GetByIdAsync(id);
            if (dock == null) return null;

            _dockRepo.Remove(dock);
            return ToDetailsDto(dock);
        }

        // Helper: Convert domain to DTO
        private DockDetailsDto ToDetailsDto(Dock dock)
        {
            return new DockDetailsDto
            {
                Id = dock.Id.AsGuid(),
                Name = dock.Name,
                Length = dock.Length,
                Depth = dock.Depth,
                MaxDraft = dock.MaxDraft,
                Coordinates = dock.Location.Coordinates,
                LocationDescription = dock.Location.Description,
                AllowedVesselTypes = dock.AllowedVesselTypes.Select(v => v.Name).ToList()
            };
        }

        // Helper: Load vessel types
        private async Task<List<VesselType>> GetVesselTypes(List<Guid> ids)
        {
            var types = new List<VesselType>();
            foreach (var id in ids)
            {
                var vt = await _vesselTypeRepo.GetByIdAsync(new VesselTypeId(id));
                if (vt == null)
                    throw new BusinessRuleValidationException($"Vessel type {id} not found.");
                types.Add(vt);
            }
            return types;
        }
    }
}
