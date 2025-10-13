using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Organizations
{
    public class RepresentativeService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IRepresentativeRepository _repo;
        private readonly IOrganizationRepository _organizationRepo;

        public RepresentativeService(
            IUnitOfWork unitOfWork,
            IRepresentativeRepository repo,
            IOrganizationRepository organizationRepo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _organizationRepo = organizationRepo;
        }


        public async Task<RepresentativeDto> AddRepresentativeAsync(AddRepresentativeDto dto)
        {
            Organization org = null;

            if (dto.OrganizationId != null)
            {
                org = await _organizationRepo.GetByIdAsync(new OrganizationId(dto.OrganizationId.Value));
                if (org == null)
                    throw new BusinessRuleValidationException("Organization not found.");
            }

            var rep = new Representative(
                dto.Name,
                dto.CitizenId,
                dto.Nationality,
                dto.Email,
                dto.PhoneNumber
            );

            if (org != null)
            {
                org.AddRepresentative(rep);
            }

            await _repo.AddAsync(rep);
            await _unitOfWork.CommitAsync();

            return ToDto(rep);
        }

  
        public async Task<RepresentativeDto> UpdateRepresentativeAsync(Guid representativeId, AddRepresentativeDto dto)
        {
            var rep = await _repo.GetByIdAsync(new RepresentativeId(representativeId));
            if (rep == null)
                throw new BusinessRuleValidationException("Representative not found.");

            Organization newOrg = null;
            if (dto.OrganizationId != null)
            {
                newOrg = await _organizationRepo.GetByIdAsync(new OrganizationId(dto.OrganizationId.Value));
                if (newOrg == null)
                    throw new BusinessRuleValidationException("Organization not found.");
            }

            rep.Update(
                dto.Name,
                dto.CitizenId,
                dto.Nationality,
                dto.Email,
                dto.PhoneNumber,
                newOrg
            );

            await _unitOfWork.CommitAsync();
            return ToDto(rep);
        }


        public async Task<RepresentativeDto> DeactivateRepresentativeAsync(Guid representativeId)
        {
            var rep = await _repo.GetByIdAsync(new RepresentativeId(representativeId));
            if (rep == null)
                throw new BusinessRuleValidationException("Representative not found.");

            rep.Deactivate();
            await _unitOfWork.CommitAsync();
            return ToDto(rep);
        }


        public async Task<RepresentativeDto> ActivateRepresentativeAsync(Guid representativeId)
        {
            var rep = await _repo.GetByIdAsync(new RepresentativeId(representativeId));
            if (rep == null)
                throw new BusinessRuleValidationException("Representative not found.");

            rep.Activate();
            await _unitOfWork.CommitAsync();
            return ToDto(rep);
        }


        public async Task<List<RepresentativeDto>> GetAllAsync()
        {
            var reps = await _repo.GetAllAsync();
            return reps.Select(ToDto).ToList();
        }

        public async Task<RepresentativeDto> GetByIdAsync(Guid id)
        {
            var rep = await _repo.GetByIdAsync(new RepresentativeId(id));
            return rep == null ? null : ToDto(rep);
        }
        
        private RepresentativeDto ToDto(Representative rep)
        {
            return new RepresentativeDto
            {
                Id = rep.Id.AsGuid(),
                Name = rep.Name,
                CitizenId = rep.CitizenId,
                Nationality = rep.Nationality,
                Email = rep.Email,
                PhoneNumber = rep.PhoneNumber,
                OrganizationId = rep.OrganizationId?.AsGuid(),
                Status = rep.Status.ToString()
            };
        }
    }
}
