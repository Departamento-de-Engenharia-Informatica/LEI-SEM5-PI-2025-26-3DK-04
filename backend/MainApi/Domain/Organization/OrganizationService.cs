using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Authentication;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Organizations
{
    public class OrganizationService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IOrganizationRepository _repo;
        private readonly IRepresentativeRepository _repRepo;
        private readonly IUserRepository _userRepo;
        private readonly UserService _userService;

        public OrganizationService(IUnitOfWork unitOfWork, IOrganizationRepository repo,
            IRepresentativeRepository repRepo, IUserRepository userRepo, UserService userService)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _repRepo = repRepo;
            _userRepo = userRepo;
            _userService = userService;
        }


        public async Task<OrganizationDto> RegisterOrganizationAsync(OrganizationDto dto)
        {
            if (string.IsNullOrWhiteSpace(dto.Id))
                throw new BusinessRuleValidationException("Organization ID is required.");

            // Validar existência da org
            if (await _repo.GetByIdAsync(new OrganizationId(dto.Id)) != null)
                throw new BusinessRuleValidationException($"An organization with ID '{dto.Id}' already exists.");

            if (await _repo.GetByLegalNameAsync(dto.LegalName) != null)
                throw new BusinessRuleValidationException("An organization with this legal name already exists.");

            if (await _repo.GetByTaxNumberAsync(dto.TaxNumber) != null)
                throw new BusinessRuleValidationException("An organization with this tax number already exists.");

            var org = new Organization(dto.Id, dto.LegalName, dto.AlternativeName, dto.Address, dto.TaxNumber);

            var repsToCreateUser = new List<(string Email, string Name)>();

            if (dto.Representatives != null && dto.Representatives.Any())
            {
                foreach (var repDto in dto.Representatives)
                {
                    // Validar Representative
                    if (await _repRepo.ExistsWithEmailAsync(repDto.Email))
                        throw new BusinessRuleValidationException("Email already in use by another representative.");

                    if (await _repRepo.ExistsWithPhoneAsync(repDto.PhoneNumber))
                        throw new BusinessRuleValidationException(
                            "Phone number already in use by another representative.");

                    if (await _repRepo.ExistsWithCidAsync(repDto.CitizenId))
                        throw new BusinessRuleValidationException(
                            "Citizen Id already in use by another representative.");
                    
                    if (await _userRepo.GetByEmailAsync(repDto.Email) != null)
                        throw new BusinessRuleValidationException("Email already in use by a system user.");

                    var rep = new Representative(
                        repDto.Name,
                        repDto.CitizenId,
                        repDto.Nationality,
                        repDto.Email,
                        repDto.PhoneNumber
                    );

                    org.AddRepresentative(rep);
                    rep.AssignToOrganization(org.Id);

                    // Guardar para criar o User depois
                    repsToCreateUser.Add((repDto.Email, repDto.Name));
                }
            }

            org.ValidateReadyForRegistration();

            // 1º passo: criar org e reps
            await _repo.AddAsync(org);
            await _unitOfWork.CommitAsync();

            // 2º passo: criar users e tokens (fora da transaction)
            foreach (var repData in repsToCreateUser)
                await _userService.CreateRepresentativeUserAsync(repData.Email, repData.Name);

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


        public async Task<bool> LegalNameExistsAsync(string legalName)
        {
            var org = await _repo.GetByLegalNameAsync(legalName);
            return org != null;
        }

        public async Task<bool> TaxNumberExistsAsync(string taxNumber)
        {
            var org = await _repo.GetByTaxNumberAsync(taxNumber);
            return org != null;
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
                    CitizenId = r.Id.AsString(),
                    Nationality = r.Nationality,
                    Email = r.Email,
                    PhoneNumber = r.PhoneNumber
                }).ToList()
            };
        }
    }
}