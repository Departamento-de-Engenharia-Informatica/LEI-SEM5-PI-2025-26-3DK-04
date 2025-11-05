using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels
{
    public class VesselTypeService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IVesselTypeRepository _repo;

        public VesselTypeService(IUnitOfWork unitOfWork, IVesselTypeRepository repo)
        {
            this._unitOfWork = unitOfWork;
            this._repo = repo;
        }

        public async Task<List<VesselTypeDto>> GetAllAsync()
        {
            var list = await this._repo.GetAllAsync();

            List<VesselTypeDto> listDto = list.ConvertAll<VesselTypeDto>(vt => new VesselTypeDto
            {
                Id = vt.Id.AsGuid(),
                Name = vt.Name,
                Description = vt.Description,
                Capacity = vt.Capacity,
                MaxRows = vt.MaxRows,
                MaxBays = vt.MaxBays,
                MaxTiers = vt.MaxTiers,
                Active = vt.Active
            });

            return listDto;
        }

        public async Task<VesselTypeDto> GetByIdAsync(VesselTypeId id)
        {
            var vesselType = await this._repo.GetByIdAsync(id);

            if (vesselType == null)
                return null;

            return new VesselTypeDto
            {
                Id = vesselType.Id.AsGuid(),
                Name = vesselType.Name,
                Description = vesselType.Description,
                Capacity = vesselType.Capacity,
                MaxRows = vesselType.MaxRows,
                MaxBays = vesselType.MaxBays,
                MaxTiers = vesselType.MaxTiers,
                Active = vesselType.Active
            };
        }

        public async Task<List<VesselTypeDto>> SearchByNameAsync(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                return new List<VesselTypeDto>();

            var list = await this._repo.SearchByNameAsync(name);

            List<VesselTypeDto> listDto = list.ConvertAll<VesselTypeDto>(vt => new VesselTypeDto
            {
                Id = vt.Id.AsGuid(),
                Name = vt.Name,
                Description = vt.Description,
                Capacity = vt.Capacity,
                MaxRows = vt.MaxRows,
                MaxBays = vt.MaxBays,
                MaxTiers = vt.MaxTiers,
                Active = vt.Active
            });

            return listDto;
        }

        public async Task<List<VesselTypeDto>> SearchByDescriptionAsync(string description)
        {
            if (string.IsNullOrWhiteSpace(description))
                return new List<VesselTypeDto>();

            var list = await this._repo.SearchByDescriptionAsync(description);

            List<VesselTypeDto> listDto = list.ConvertAll<VesselTypeDto>(vt => new VesselTypeDto
            {
                Id = vt.Id.AsGuid(),
                Name = vt.Name,
                Description = vt.Description,
                Capacity = vt.Capacity,
                MaxRows = vt.MaxRows,
                MaxBays = vt.MaxBays,
                MaxTiers = vt.MaxTiers,
                Active = vt.Active
            });

            return listDto;
        }

        public async Task<List<VesselTypeDto>> SearchAsync(string searchTerm)
        {
            if (string.IsNullOrWhiteSpace(searchTerm))
                return await GetAllAsync();

            var list = await this._repo.SearchByNameOrDescriptionAsync(searchTerm);

            List<VesselTypeDto> listDto = list.ConvertAll<VesselTypeDto>(vt => new VesselTypeDto
            {
                Id = vt.Id.AsGuid(),
                Name = vt.Name,
                Description = vt.Description,
                Capacity = vt.Capacity,
                MaxRows = vt.MaxRows,
                MaxBays = vt.MaxBays,
                MaxTiers = vt.MaxTiers,
                Active = vt.Active
            });

            return listDto;
        }

        public async Task<VesselTypeDto> AddAsync(CreatingVesselTypeDto dto)
        {
            var vesselType = new VesselType(
                dto.Name,
                dto.Description,
                dto.Capacity,
                dto.MaxRows,
                dto.MaxBays,
                dto.MaxTiers
            );

            await this._repo.AddAsync(vesselType);
            await this._unitOfWork.CommitAsync();

            return new VesselTypeDto
            {
                Id = vesselType.Id.AsGuid(),
                Name = vesselType.Name,
                Description = vesselType.Description,
                Capacity = vesselType.Capacity,
                MaxRows = vesselType.MaxRows,
                MaxBays = vesselType.MaxBays,
                MaxTiers = vesselType.MaxTiers,
                Active = vesselType.Active
            };
        }

        public async Task<VesselTypeDto> UpdateAsync(Guid id, UpdatingVesselTypeDto dto)
        {
            var vesselType = await this._repo.GetByIdAsync(new VesselTypeId(id));

            if (vesselType == null)
                return null;

            // Update all fields
            vesselType.ChangeName(dto.Name);
            vesselType.ChangeDescription(dto.Description);
            vesselType.ChangeCapacity(dto.Capacity);
            vesselType.ChangeOperationalConstraints(dto.MaxRows, dto.MaxBays, dto.MaxTiers);

            await this._unitOfWork.CommitAsync();

            return new VesselTypeDto
            {
                Id = vesselType.Id.AsGuid(),
                Name = vesselType.Name,
                Description = vesselType.Description,
                Capacity = vesselType.Capacity,
                MaxRows = vesselType.MaxRows,
                MaxBays = vesselType.MaxBays,
                MaxTiers = vesselType.MaxTiers,
                Active = vesselType.Active
            };
        }

        public async Task<VesselTypeDto> InactivateAsync(VesselTypeId id)
        {
            var vesselType = await this._repo.GetByIdAsync(id);

            if (vesselType == null)
                return null;

            vesselType.MarkAsInactive();

            await this._unitOfWork.CommitAsync();

            return new VesselTypeDto
            {
                Id = vesselType.Id.AsGuid(),
                Name = vesselType.Name,
                Description = vesselType.Description,
                Capacity = vesselType.Capacity,
                MaxRows = vesselType.MaxRows,
                MaxBays = vesselType.MaxBays,
                MaxTiers = vesselType.MaxTiers,
                Active = vesselType.Active
            };
        }

        public async Task<VesselTypeDto> ActivateAsync(VesselTypeId id)
        {
            var vesselType = await this._repo.GetByIdAsync(id);

            if (vesselType == null)
                return null;

            vesselType.MarkAsActive();

            await this._unitOfWork.CommitAsync();

            return new VesselTypeDto
            {
                Id = vesselType.Id.AsGuid(),
                Name = vesselType.Name,
                Description = vesselType.Description,
                Capacity = vesselType.Capacity,
                MaxRows = vesselType.MaxRows,
                MaxBays = vesselType.MaxBays,
                MaxTiers = vesselType.MaxTiers,
                Active = vesselType.Active
            };
        }

        public async Task<VesselTypeDto> DeleteAsync(VesselTypeId id)
        {
            var vesselType = await this._repo.GetByIdAsync(id);

            if (vesselType == null)
                return null;

            this._repo.Remove(vesselType);
            await this._unitOfWork.CommitAsync();

            return new VesselTypeDto
            {
                Id = vesselType.Id.AsGuid(),
                Name = vesselType.Name,
                Description = vesselType.Description,
                Capacity = vesselType.Capacity,
                MaxRows = vesselType.MaxRows,
                MaxBays = vesselType.MaxBays,
                MaxTiers = vesselType.MaxTiers,
                Active = vesselType.Active
            };
        }
    }
}
