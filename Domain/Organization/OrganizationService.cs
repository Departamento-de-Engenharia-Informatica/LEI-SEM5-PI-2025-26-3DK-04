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
        private readonly IOrganizationRepository _repo;
        private readonly IRepresentativeRepository _repRepo;

        public OrganizationService(IUnitOfWork unitOfWork, IOrganizationRepository repo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
        }


        public async Task<OrganizationDto> RegisterOrganizationAsync(OrganizationDto dto)
        {
            if (string.IsNullOrWhiteSpace(dto.Id))
                throw new BusinessRuleValidationException("Organization ID is required.");

            // Validar se já existe uma organização com este ID
            var existingById = await _repo.GetByIdAsync(new OrganizationId(dto.Id));
            if (existingById != null)
                throw new BusinessRuleValidationException($"An organization with ID '{dto.Id}' already exists.");

            if (await _repo.ExistsWithLegalNameAsync(dto.LegalName))
                throw new BusinessRuleValidationException("An organization with this legal name already exists.");

            var org = new Organization(dto.Id, dto.LegalName, dto.AlternativeName, dto.Address, dto.TaxNumber);


            if (dto.Representatives != null && dto.Representatives.Any())
            {
                foreach (var repDto in dto.Representatives)
                {
                    if (await _repRepo.ExistsWithEmailAsync(repDto.Email))
                        throw new BusinessRuleValidationException("Email already in use by another representative.");

                    if (await _repRepo.ExistsWithPhoneAsync(repDto.PhoneNumber))
                        throw new BusinessRuleValidationException("Phone number already in use by another representative.");
                    if(await _repRepo.ExistsWithCidAsync(repDto.CitizenId))
                        throw new BusinessRuleValidationException("Citizen Id already in use by another representative.");
                    var rep = new Representative(repDto.Name, repDto.CitizenId, repDto.Nationality, repDto.Email,
                        repDto.PhoneNumber);
                    org.AddRepresentative(rep);
                }
            }


            org.ValidateReadyForRegistration();

            await _repo.AddAsync(org);
            await _unitOfWork.CommitAsync();

            return ToDto(org);
        }
        
        public async Task<List<OrganizationDto>> GetAllAsync()
        {
            var orgs = await _repo.GetAllAsync();
            return orgs.Select(ToDto).ToList();
        }


        public async Task<OrganizationDto> GetByIdAsync(string id)
        {
            var org = await _repo.GetByIdAsync(new OrganizationId(id));
            return org == null ? null : ToDto(org);
        }


        private static OrganizationDto ToDto(Organization org)
        {
            return new OrganizationDto
            {
                Id = org.Id.AsString(),
                LegalName = org.LegalName,
                AlternativeName = org.AlternativeName,
                Address = org.Address,
                TaxNumber = org.TaxNumber,
                Representatives = org.Representatives.Select(r => new AddRepresentativeToOrgDto()
                {
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