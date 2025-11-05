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
            if (await _repo.ExistsWithEmailAsync(dto.Email))
                throw new BusinessRuleValidationException("Email already in use by another representative.");
            if (await _repo.ExistsWithPhoneAsync(dto.PhoneNumber))
                throw new BusinessRuleValidationException("Phone number already in use by another representative.");
            if (await _repo.ExistsWithCidAsync(dto.CitizenId))
                throw new BusinessRuleValidationException("Citizen Id already in use by another representative.");

            var rep = new Representative(
                dto.Name,
                dto.CitizenId,
                dto.Nationality,
                dto.Email,
                dto.PhoneNumber
            );

            if (!string.IsNullOrWhiteSpace(dto.OrganizationId))
            {
                var org = await _organizationRepo.GetByIdAsync(new OrganizationId(dto.OrganizationId));
                if (org == null)
                    throw new BusinessRuleValidationException("Organization not found.");

                // 1. Atribuir OrganizationId imediatamente
                rep.AssignToOrganization(org.Id);

                // 2. Adicionar à organização
                org.AddRepresentative(rep);
            }
            else
            {
                throw new BusinessRuleValidationException("Representative must be assigned to an organization.");
            }

            await _repo.AddAsync(rep);
            await _unitOfWork.CommitAsync();

            return ToDto(rep);
        }
        
        public async Task<RepresentativeDto> UpdateRepresentativeAsync(string currentCitizenId, UpdateRepresentativeDto dto)
        {
            // Obter o representante existente
            var rep = await _repo.GetByIdAsync(new RepresentativeId(currentCitizenId));
            if (rep == null)
                throw new BusinessRuleValidationException("Representative not found.");

            // Se o CitizenId mudou
            if (currentCitizenId != dto.CitizenId)
            {
                // Verifica se o novo CitizenId já existe
                if (await _repo.ExistsWithCidAsync(dto.CitizenId))
                    throw new BusinessRuleValidationException("The new CitizenId is already in use.");

                // Criar novo representante usando a lógica de AddRepresentativeAsync
                var newRepDto = new AddRepresentativeDto
                {
                    Name = dto.Name,
                    CitizenId = dto.CitizenId,
                    Nationality = dto.Nationality,
                    Email = dto.Email,
                    PhoneNumber = dto.PhoneNumber,
                    OrganizationId = rep.OrganizationId.AsString()
                };

                var newRep = await AddRepresentativeAsync(newRepDto);

                // Remover o representante antigo da organização e do repositório
                var org = await _organizationRepo.GetByIdAsync(rep.OrganizationId);
                if (org != null)
                {
                    org.RemoveRepresentative(rep);
                    await _organizationRepo.UpdateAsync(org);
                }

                await _repo.DeleteAsync(rep);

                // Retornar DTO do novo representante
                return newRep;
            }else {
                // Apenas atualiza os campos do representante existente
                rep.ChangeName(dto.Name);
                rep.ChangeEmail(dto.Email);
                rep.ChangePhoneNumber(dto.PhoneNumber);
                rep.ChangeNationality(dto.Nationality);

                await _repo.UpdateAsync(rep);
                await _unitOfWork.CommitAsync();

                return ToDto(rep);
            }
        }



        public async Task<RepresentativeDto> DeactivateRepresentativeAsync(string representativeId)
        {
            var rep = await _repo.GetByIdAsync(new RepresentativeId(representativeId));
            if (rep == null)
                throw new BusinessRuleValidationException("Representative not found.");

            rep.Deactivate();
            await _unitOfWork.CommitAsync();
            return ToDto(rep);
        }


        public async Task<RepresentativeDto> ActivateRepresentativeAsync(string representativeId)
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

        public async Task<RepresentativeDto> GetByIdAsync(string id)
        {
            var rep = await _repo.GetByIdAsync(new RepresentativeId(id));
            return rep == null ? null : ToDto(rep);
        }
        
        private RepresentativeDto ToDto(Representative rep)
        {
            return new RepresentativeDto
            {
                
                Name = rep.Name,
                CitizenId = rep.Id.AsString(),
                Nationality = rep.Nationality,
                Email = rep.Email,
                PhoneNumber = rep.PhoneNumber,
                OrganizationId = rep.OrganizationId?.AsString(),
                Status = rep.Status.ToString()
            };
        }
    }
}
