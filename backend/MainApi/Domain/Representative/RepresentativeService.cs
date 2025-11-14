using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Authentication;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Organizations
{
    public class RepresentativeService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IRepresentativeRepository _repo;
        private readonly IOrganizationRepository _organizationRepo;
        private readonly IUserRepository _userRepo;
        private readonly UserService _userService;

        public RepresentativeService(
            IUnitOfWork unitOfWork,
            IRepresentativeRepository repo,
            IOrganizationRepository organizationRepo,
            IUserRepository userRepo,
            UserService userService)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _organizationRepo = organizationRepo;
            _userRepo = userRepo;
            _userService = userService;
        }

        // ------------------------------------------------------------------------------------
        //                                  CREATE
        // ------------------------------------------------------------------------------------
        public async Task<RepresentativeDto> AddRepresentativeAsync(AddRepresentativeDto dto)
        {
            // 1) Cross-validation email
            await EnsureEmailIsUniqueAsync(dto.Email);

            // 2) Rep-specific validations
            if (await _repo.ExistsWithPhoneAsync(dto.PhoneNumber))
                throw new BusinessRuleValidationException("Phone number already in use.");

            if (await _repo.ExistsWithCidAsync(dto.CitizenId))
                throw new BusinessRuleValidationException("Citizen ID already in use.");

            // 3) Validate org
            var org = await _organizationRepo.GetByIdAsync(new OrganizationId(dto.OrganizationId));
            if (org == null)
                throw new BusinessRuleValidationException("Organization not found.");

            // 4) Create representative domain object
            var rep = new Representative(
                dto.Name,
                dto.CitizenId,
                dto.Nationality,
                dto.Email,
                dto.PhoneNumber
            );

            rep.AssignToOrganization(org.Id);
            org.AddRepresentative(rep);

            // 5) Create User (via UserService) with proper token & pending activation
            

            // 6) Persist everything in one atomic transaction
            await _repo.AddAsync(rep);
            await _unitOfWork.CommitAsync();
            
            await _userService.CreateRepresentativeUserAsync(dto.Email, dto.Name);
            // 7) Done
            return ToDto(rep);
        }

        private async Task EnsureEmailIsUniqueAsync(string email)
        {
            if (await _repo.ExistsWithEmailAsync(email))
                throw new BusinessRuleValidationException("Email already used by a representative.");

            if (await _userRepo.GetByEmailAsync(email) != null)
                throw new BusinessRuleValidationException("Email already used by a system user.");
        }

        // ------------------------------------------------------------------------------------
        //                                  UPDATE
        // ------------------------------------------------------------------------------------
        public async Task<RepresentativeDto> UpdateRepresentativeAsync(string currentCitizenId, UpdateRepresentativeDto dto)
        {
            var rep = await _repo.GetByIdAsync(new RepresentativeId(currentCitizenId));
            if (rep == null)
                throw new BusinessRuleValidationException("Representative not found.");

            bool idChanged = currentCitizenId != dto.CitizenId;

            // Email change → must be unique across users + reps
            if (dto.Email != rep.Email)
                await EnsureEmailIsUniqueAsync(dto.Email);

            if (idChanged)
            {
                // Validate new citizen ID
                if (await _repo.ExistsWithCidAsync(dto.CitizenId))
                    throw new BusinessRuleValidationException("The new CitizenId is already in use.");

                // Create new rep through AddRepresentativeAsync
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

                // Remove old rep
                var org = await _organizationRepo.GetByIdAsync(rep.OrganizationId);
                if (org != null)
                {
                    org.RemoveRepresentative(rep);
                    await _organizationRepo.UpdateAsync(org);
                }

                await _repo.DeleteAsync(rep);
                await _unitOfWork.CommitAsync();

                return newRep;
            }

            // If ID was not changed → just update fields
            rep.ChangeName(dto.Name);
            rep.ChangeEmail(dto.Email);
            rep.ChangePhoneNumber(dto.PhoneNumber);
            rep.ChangeNationality(dto.Nationality);

            await _repo.UpdateAsync(rep);
            await _unitOfWork.CommitAsync();

            return ToDto(rep);
        }

        // ------------------------------------------------------------------------------------
        //                                  STATUS TOGGLE
        // ------------------------------------------------------------------------------------
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

        // ------------------------------------------------------------------------------------
        //                                  GETTERS
        // ------------------------------------------------------------------------------------
        public async Task<List<RepresentativeDto>> GetAllAsync()
        {
            var reps = await _repo.GetAllAsync();
            return reps.Select(ToDto).ToList();
        }

        public async Task<List<RepresentativeDto>> GetActiveAsync()
        {
            var reps = await _repo.GetAllAsync();
            return reps.Where(r => r.Status == RepresentativeStatus.Active)
                       .Select(ToDto).ToList();
        }

        public async Task<List<RepresentativeDto>> GetInactiveAsync()
        {
            var reps = await _repo.GetAllAsync();
            return reps.Where(r => r.Status == RepresentativeStatus.Inactive)
                       .Select(ToDto).ToList();
        }

        public async Task<RepresentativeDto> GetByIdAsync(string id)
        {
            var rep = await _repo.GetByIdAsync(new RepresentativeId(id));
            return rep == null ? null : ToDto(rep);
        }
        public async Task<bool> EmailExistsAsync(string email)
        {
            var rep = await _repo.GetRepresentativeByEmailAsync(email);
            return rep != null;
        }

        public async Task<bool> CitizenIdExistsAsync(string cid)
        {
            var rep = await _repo.GetRepresentativeByCitizenCardAsync(cid);
            return rep != null;
        }

        public async Task<bool> PhoneExistsAsync(string phone)
        {
            var rep = await _repo.GetRepresentativeByPhoneAsync(phone);
            return rep != null;
        }

        // ------------------------------------------------------------------------------------
        //                                  DTO
        // ------------------------------------------------------------------------------------
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
