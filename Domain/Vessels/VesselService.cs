using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels
{
    public class VesselService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IVesselRepository _vesselRepo;
        private readonly IVesselTypeRepository _vesselTypeRepo;

        public VesselService(IUnitOfWork unitOfWork, IVesselRepository vesselRepo, IVesselTypeRepository vesselTypeRepo)
        {
            this._unitOfWork = unitOfWork;
            this._vesselRepo = vesselRepo;
            this._vesselTypeRepo = vesselTypeRepo;
        }

        public async Task<List<VesselDto>> GetAllAsync()
        {
            var list = await this._vesselRepo.GetAllAsync();
            var vesselDtos = new List<VesselDto>();

            foreach (var vessel in list)
            {
                var vesselType = await this._vesselTypeRepo.GetByIdAsync(vessel.VesselTypeId);
                vesselDtos.Add(new VesselDto
                {
                    Id = vessel.Id.AsGuid(),
                    ImoNumber = vessel.ImoNumber.ToString(),
                    Name = vessel.Name,
                    VesselTypeId = vessel.VesselTypeId.AsGuid(),
                    VesselTypeName = vesselType?.Name,
                    Owner = vessel.Owner,
                    Operator = vessel.Operator,
                    Active = vessel.Active
                });
            }

            return vesselDtos;
        }

        public async Task<VesselDto> GetByIdAsync(VesselId id)
        {
            var vessel = await this._vesselRepo.GetByIdAsync(id);

            if (vessel == null)
                return null;

            var vesselType = await this._vesselTypeRepo.GetByIdAsync(vessel.VesselTypeId);

            return new VesselDto
            {
                Id = vessel.Id.AsGuid(),
                ImoNumber = vessel.ImoNumber.ToString(),
                Name = vessel.Name,
                VesselTypeId = vessel.VesselTypeId.AsGuid(),
                VesselTypeName = vesselType?.Name,
                Owner = vessel.Owner,
                Operator = vessel.Operator,
                Active = vessel.Active
            };
        }

        public async Task<VesselDto> GetByImoNumberAsync(string imoNumber)
        {
            var imo = new ImoNumber(imoNumber);
            var vessel = await this._vesselRepo.GetByImoNumberAsync(imo);

            if (vessel == null)
                return null;

            var vesselType = await this._vesselTypeRepo.GetByIdAsync(vessel.VesselTypeId);

            return new VesselDto
            {
                Id = vessel.Id.AsGuid(),
                ImoNumber = vessel.ImoNumber.ToString(),
                Name = vessel.Name,
                VesselTypeId = vessel.VesselTypeId.AsGuid(),
                VesselTypeName = vesselType?.Name,
                Owner = vessel.Owner,
                Operator = vessel.Operator,
                Active = vessel.Active
            };
        }

        public async Task<List<VesselDto>> SearchByNameAsync(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                return new List<VesselDto>();

            var list = await this._vesselRepo.SearchByNameAsync(name);
            return await MapToVesselDtos(list);
        }

        public async Task<List<VesselDto>> SearchByOwnerAsync(string owner)
        {
            if (string.IsNullOrWhiteSpace(owner))
                return new List<VesselDto>();

            var list = await this._vesselRepo.SearchByOwnerAsync(owner);
            return await MapToVesselDtos(list);
        }

        public async Task<List<VesselDto>> SearchByOperatorAsync(string operatorName)
        {
            if (string.IsNullOrWhiteSpace(operatorName))
                return new List<VesselDto>();

            var list = await this._vesselRepo.SearchByOperatorAsync(operatorName);
            return await MapToVesselDtos(list);
        }

        public async Task<List<VesselDto>> SearchAsync(string searchTerm)
        {
            if (string.IsNullOrWhiteSpace(searchTerm))
                return await GetAllAsync();

            var list = await this._vesselRepo.SearchAsync(searchTerm);
            return await MapToVesselDtos(list);
        }

        private async Task<List<VesselDto>> MapToVesselDtos(List<Vessel> vessels)
        {
            var vesselDtos = new List<VesselDto>();

            foreach (var vessel in vessels)
            {
                var vesselType = await this._vesselTypeRepo.GetByIdAsync(vessel.VesselTypeId);
                vesselDtos.Add(new VesselDto
                {
                    Id = vessel.Id.AsGuid(),
                    ImoNumber = vessel.ImoNumber.ToString(),
                    Name = vessel.Name,
                    VesselTypeId = vessel.VesselTypeId.AsGuid(),
                    VesselTypeName = vesselType?.Name,
                    Owner = vessel.Owner,
                    Operator = vessel.Operator,
                    Active = vessel.Active
                });
            }

            return vesselDtos;
        }

        public async Task<VesselDto> AddAsync(CreatingVesselDto dto)
        {
            // Validate IMO number format
            var imoNumber = new ImoNumber(dto.ImoNumber);

            // Check if vessel with this IMO already exists
            if (await this._vesselRepo.ExistsByImoNumberAsync(imoNumber))
            {
                throw new BusinessRuleValidationException(
                    $"A vessel with IMO number {imoNumber} already exists.");
            }

            // Validate vessel type exists
            var vesselType = await this._vesselTypeRepo.GetByIdAsync(new VesselTypeId(dto.VesselTypeId));
            if (vesselType == null)
            {
                throw new BusinessRuleValidationException(
                    $"Vessel type with ID {dto.VesselTypeId} does not exist.");
            }

            var vessel = new Vessel(
                imoNumber,
                dto.Name,
                new VesselTypeId(dto.VesselTypeId),
                dto.Owner,
                dto.Operator
            );

            await this._vesselRepo.AddAsync(vessel);
            await this._unitOfWork.CommitAsync();

            return new VesselDto
            {
                Id = vessel.Id.AsGuid(),
                ImoNumber = vessel.ImoNumber.ToString(),
                Name = vessel.Name,
                VesselTypeId = vessel.VesselTypeId.AsGuid(),
                VesselTypeName = vesselType.Name,
                Owner = vessel.Owner,
                Operator = vessel.Operator,
                Active = vessel.Active
            };
        }

        public async Task<VesselDto> UpdateAsync(Guid id, UpdatingVesselDto dto)
        {
            var vessel = await this._vesselRepo.GetByIdAsync(new VesselId(id));

            if (vessel == null)
                return null;

            // Validate vessel type exists
            var vesselType = await this._vesselTypeRepo.GetByIdAsync(new VesselTypeId(dto.VesselTypeId));
            if (vesselType == null)
            {
                throw new BusinessRuleValidationException(
                    $"Vessel type with ID {dto.VesselTypeId} does not exist.");
            }

            // Update vessel properties
            vessel.ChangeName(dto.Name);
            vessel.ChangeVesselType(new VesselTypeId(dto.VesselTypeId));
            vessel.ChangeOwner(dto.Owner);
            vessel.ChangeOperator(dto.Operator);

            await this._unitOfWork.CommitAsync();

            return new VesselDto
            {
                Id = vessel.Id.AsGuid(),
                ImoNumber = vessel.ImoNumber.ToString(),
                Name = vessel.Name,
                VesselTypeId = vessel.VesselTypeId.AsGuid(),
                VesselTypeName = vesselType.Name,
                Owner = vessel.Owner,
                Operator = vessel.Operator,
                Active = vessel.Active
            };
        }

        public async Task<VesselDto> InactivateAsync(VesselId id)
        {
            var vessel = await this._vesselRepo.GetByIdAsync(id);

            if (vessel == null)
                return null;

            vessel.MarkAsInactive();
            await this._unitOfWork.CommitAsync();

            var vesselType = await this._vesselTypeRepo.GetByIdAsync(vessel.VesselTypeId);

            return new VesselDto
            {
                Id = vessel.Id.AsGuid(),
                ImoNumber = vessel.ImoNumber.ToString(),
                Name = vessel.Name,
                VesselTypeId = vessel.VesselTypeId.AsGuid(),
                VesselTypeName = vesselType?.Name,
                Owner = vessel.Owner,
                Operator = vessel.Operator,
                Active = vessel.Active
            };
        }

        public async Task<VesselDto> ActivateAsync(VesselId id)
        {
            var vessel = await this._vesselRepo.GetByIdAsync(id);

            if (vessel == null)
                return null;

            vessel.MarkAsActive();
            await this._unitOfWork.CommitAsync();

            var vesselType = await this._vesselTypeRepo.GetByIdAsync(vessel.VesselTypeId);

            return new VesselDto
            {
                Id = vessel.Id.AsGuid(),
                ImoNumber = vessel.ImoNumber.ToString(),
                Name = vessel.Name,
                VesselTypeId = vessel.VesselTypeId.AsGuid(),
                VesselTypeName = vesselType?.Name,
                Owner = vessel.Owner,
                Operator = vessel.Operator,
                Active = vessel.Active
            };
        }

        public async Task<VesselDto> DeleteAsync(VesselId id)
        {
            var vessel = await this._vesselRepo.GetByIdAsync(id);

            if (vessel == null)
                return null;

            var vesselType = await this._vesselTypeRepo.GetByIdAsync(vessel.VesselTypeId);

            this._vesselRepo.Remove(vessel);
            await this._unitOfWork.CommitAsync();

            return new VesselDto
            {
                Id = vessel.Id.AsGuid(),
                ImoNumber = vessel.ImoNumber.ToString(),
                Name = vessel.Name,
                VesselTypeId = vessel.VesselTypeId.AsGuid(),
                VesselTypeName = vesselType?.Name,
                Owner = vessel.Owner,
                Operator = vessel.Operator,
                Active = vessel.Active
            };
        }
    }
}
