using System;
using System.Linq;
using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Organizations;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Authentication
{
    public class UserService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IUserRepository _repo;
        private readonly IUserActivationRepository _activationRepo;
        private readonly EmailService _emailService;

        public UserService(
            IUnitOfWork unitOfWork,
            IUserRepository repo,
            IUserActivationRepository activationRepo,
            EmailService emailService)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _activationRepo = activationRepo;
            _emailService = emailService;
        }

        // -------------------------
        //        DTOs
        // -------------------------

        public class CreateUserDTO
        {
            public string Email { get; set; }
            public string Name { get; set; }
            public string Picture { get; set; }
            public Roles Role { get; set; }
        }

        public class UpdateRoleDTO
        {
            public Roles Role { get; set; }
        }

        public class UserDto
        {
            public string Email { get; set; }
            public string Name { get; set; }
            public string Picture { get; set; }
            public string Role { get; set; }
            public string Status { get; set; }
        }

        // -------------------------
        //     Métodos DDD
        // -------------------------

        public async Task<(bool Exists, bool IsValid, string Error)> CheckUserExistsAsync(string email)
        {
            if (string.IsNullOrWhiteSpace(email))
                return (false, false, "Invalid email");

            var user = await _repo.GetByIdAsync(new UserID(email));
            return (user != null, true, null);
        }


        public async Task<(bool Success, string Error)> CreateUserAsync(CreateUserDTO dto)
        {
            var exists = await _repo.GetByIdAsync(new UserID(dto.Email));
            if (exists != null)
                return (false, "A user with this email already exists.");

            var user = new User(dto.Email, dto.Name, dto.Picture, dto.Role, Status.Inactive);
            await _repo.AddAsync(user);

            var activation = new UserActivation(user.Id);
            await _activationRepo.AddAsync(activation);

            var link = $"https://localhost:5001/api/UserManagement/activate?token={activation.Token}";
            _emailService.SendActivationEmail(dto.Email, link);
            await _unitOfWork.CommitAsync();
            
            return (true, null);
        }


        public async Task<(bool Success, string Error)> UpdateRoleAsync(string email, UpdateRoleDTO dto)
        {
            var user = await _repo.GetByIdAsync(new UserID(email));
            if (user == null)
                return (false, "User not found");

            if (dto.Role == Roles.Representative)
                return (false, "Cannot manually assign Representative role.");

            user.SetRole(dto.Role);
            user.SetStatus(Status.Inactive);
            await _repo.UpdateAsync(user);

            var activation = new UserActivation(user.Id);
            await _activationRepo.AddAsync(activation);
            
            var link = $"https://localhost:5001/api/UserManagement/activate?token={activation.Token}";
            _emailService.SendActivationEmail(email, link);
            await _unitOfWork.CommitAsync();
            
            return (true, null);
        }


        public async Task<string> ActivateUserAsync(string token)
        {
            var activation = await _activationRepo.GetByTokenAsync(token);

            if (activation == null || !activation.IsValid(token))
                return "http://localhost:4200/activate?status=error";

            var user = await _repo.GetByIdAsync(activation.UserId);
            if (user == null)
                return "http://localhost:4200/activate?status=error";

            user.Activate();
            await _repo.UpdateAsync(user);
            await _activationRepo.DeleteAsync(activation);
            await _unitOfWork.CommitAsync();
            
            return "http://localhost:4200/activate?status=success";
        }


        public async Task<(bool Success, string Error)> DeactivateUserAsync(string email)
        {
            var user = await _repo.GetByIdAsync(new UserID(email));
            if (user == null)
                return (false, "User not found");

            user.Deactivate();
            await _repo.UpdateAsync(user);
            await _unitOfWork.CommitAsync();
            
            return (true, null);
        }


        public async Task<IEnumerable<UserDto>> GetAllUsersAsync()
        {
            var users = await _repo.GetAllAsync();
            return users.Select(ToDto);
        }

        private UserDto ToDto(User u)
        {
            return new UserDto
            {
                Email = u.Id.Value,
                Name = u.Name,
                Picture = u.Picture,
                Role = u.Role.ToString(),
                Status = u.Status.ToString()
            };
        }
        
        public async Task<(bool Success, string Error)> CreateRepresentativeUserAsync(string email, string name)
        {
            if (string.IsNullOrWhiteSpace(email))
                return (false, "Invalid email.");

            // 2. Criar User com role Representative e status Inactive
            var user = new User(
                email: email,
                name: name,
                picture: " ",   // reps não têm picture nesta fase
                role: Roles.Representative,
                status: Status.Inactive
            );

            await _repo.AddAsync(user);

            // 3. Criar token de ativação
            var activation = new UserActivation(user.Id);
            await _activationRepo.AddAsync(activation);
            await _unitOfWork.CommitAsync();
            
            // 4. Enviar email de ativação
            var link = $"https://localhost:5001/api/UserManagement/activate?token={activation.Token}";
            _emailService.SendActivationEmail(email, link);

            return (true, null);
        }


    }
}
