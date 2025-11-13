using System;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using DDDSample1.Domain.Authentication;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Authorization;

namespace DDDSample1.Controllers
{
    //[AuthorizeRole(Roles.Admin)]
    [ApiController]
    [Route("api/[controller]")]
    public class UserManagementController : ControllerBase
    {
        private readonly IUserRepository _repo;
        private readonly IUserActivationRepository _activationRepo;
        private readonly EmailService _emailService;

        public UserManagementController(IUserRepository repo, IUserActivationRepository activationRepo, EmailService emailService)
        {
            _repo = repo;
            _activationRepo = activationRepo;
            _emailService = emailService;
        }
        
        [HttpGet("check/{email}")]
        public async Task<IActionResult> CheckUserPath(string email)
        {
            return await CheckUser(email);
        }
        
        [HttpGet("check")]
        public async Task<IActionResult> CheckUser([FromQuery] string email)
        {
            if (string.IsNullOrWhiteSpace(email))
                return BadRequest(new { exists = false, error = "Invalid email" });

            var user = await _repo.GetByIdAsync(new UserID(email));

            return Ok(new { exists = user != null });
        }

        
        [HttpPost("create")]
        public async Task<IActionResult> CreateUser([FromBody] CreateUserDTO dto)
        {
            Console.WriteLine($"[CreateUser] Email: {dto.Email}, Name: {dto.Name}, Role: {dto.Role}");

            var user = new User(dto.Email, dto.Name, dto.Picture, dto.Role, Status.Inactive);
            await _repo.AddAsync(user);

            var activation = new UserActivation(user.Id);
            await _activationRepo.AddAsync(activation);

            var link = $"https://localhost:5001/api/UserManagement/activate?token={activation.Token}";
            _emailService.SendActivationEmail(dto.Email, link);

            return Ok(new { message = "User created. Activation email sent." });
        }

      
        [HttpPut("{email}/role")]
        public async Task<IActionResult> UpdateRole(string email, [FromBody] UpdateRoleDTO dto)
        {
            var user = await _repo.GetByIdAsync(new UserID(email));
            if (user == null)
                return NotFound(new { error = "User not found" });

            user.SetRole(dto.Role);
            user.SetStatus(Status.Inactive);
            await _repo.UpdateAsync(user);

            var activation = new UserActivation(user.Id);
            await _activationRepo.AddAsync(activation);

            var link = $"https://localhost:5001/api/UserManagement/activate?token={activation.Token}";
            _emailService.SendActivationEmail(email, link);

            return Ok(new { message = "Role updated and activation email sent." });
        }

       
        [AllowAnonymous]
        [HttpGet("activate")]
        public async Task<IActionResult> ActivateByToken([FromQuery] string token)
        {
            var activation = await _activationRepo.GetByTokenAsync(token);

            if (activation == null || !activation.IsValid(token))
            {
                return Redirect("http://localhost:4200/activate?status=error");
            }

            // Ativa o user
            var user = await _repo.GetByEmailAsync(activation.UserId.Value);
            if (user == null)
            {
                return Redirect("http://localhost:4200/activate?status=error");
            }

            user.Activate();
            await _repo.UpdateAsync(user);
            await _activationRepo.DeleteAsync(activation);

            return Redirect("http://localhost:4200/activate?status=success");
        }


        
        [HttpPut("{email}/deactivate")]
        public async Task<IActionResult> Deactivate(string email)
        {
            var user = await _repo.GetByIdAsync(new UserID(email));
            if (user == null)
                return NotFound(new { error = "User not found" });

            user.Deactivate();
            await _repo.UpdateAsync(user);

            return Ok(new { message = "User deactivated" });
        }
    }

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
}
