using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Organizations
{
    public class OrganizationService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly OrganizationRepository _repo;

        public OrganizationService(IUnitOfWork unitOfWork, OrganizationRepository repo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
        }

        // ✅ Criar nova organização
        public async Task<OrganizationDto> RegisterOrganizationAsync(OrganizationDto dto)
        {
            if (await _repo.ExistsWithLegalNameAsync(dto.LegalName))
                throw new BusinessRuleValidationException("An organization with this legal name already exists.");

            var org = new Organization(dto.LegalName, dto.AlternativeName, dto.Address, dto.TaxNumber);

            await _repo.AddAsync(org);
            await _unitOfWork.CommitAsync();

            return ToDto(org);
        }

        // ✅ Adicionar representante a uma organização existente
        public async Task<OrganizationDto> AddRepresentativeAsync(Guid organizationId, AddRepresentativeDto dto)
        {
            var org = await _repo.GetByIdAsync(new OrganizationId(organizationId));

            if (org == null)
                throw new BusinessRuleValidationException("Organization not found.");

            var rep = new Representative(dto.Name, dto.CitizenId, dto.Nationality, dto.Email, dto.PhoneNumber);

            org.AddRepresentative(rep);

            await _unitOfWork.CommitAsync();

            return ToDto(org);
        }

        // ✅ Obter todas as organizações
        public async Task<List<OrganizationDto>> GetAllAsync()
        {
            var orgs = await _repo.GetAllAsync();
            return orgs.Select(ToDto).ToList();
        }

        // ✅ Obter por ID
        public async Task<OrganizationDto> GetByIdAsync(Guid id)
        {
            var org = await _repo.GetByIdAsync(new OrganizationId(id));
            return org == null ? null : ToDto(org);
        }

        // 🔧 Conversão para DTO
        private static OrganizationDto ToDto(Organization org)
        {
            return new OrganizationDto
            {
                Id = org.Id.AsGuid(),
                LegalName = org.LegalName,
                AlternativeName = org.AlternativeName,
                Address = org.Address,
                TaxNumber = org.TaxNumber,
                Representatives = org.Representatives.Select(r => new RepresentativeDto
                {
                    Id = r.Id.AsGuid(),
                    Name = r.Name,
                    CitizenId = r.CitizenId,
                    Nationality = r.Nationality,
                    Email = r.Email,
                    PhoneNumber = r.PhoneNumber
                }).ToList()
            };
        }
    }
}
