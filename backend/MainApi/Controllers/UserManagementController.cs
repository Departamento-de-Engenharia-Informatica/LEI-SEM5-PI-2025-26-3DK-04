using System;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using DDDSample1.Domain.Authentication;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Authorization;

namespace DDDSample1.Controllers
{
    [ApiController]
    [Route("api/[controller]")]
    public class UserManagementController : ControllerBase
    {
        private readonly UserService _service;

        public UserManagementController(UserService service)
        {
            _service = service;
        }

        [HttpGet("check/{email}")]
        public async Task<IActionResult> CheckUserPath(string email)
        {
            return await CheckUser(email);
        }

        [HttpGet("check")]
        public async Task<IActionResult> CheckUser([FromQuery] string email)
        {
            var result = await _service.CheckUserExistsAsync(email);
            if (!result.IsValid)
                return BadRequest(new { exists = false, error = result.Error });

            return Ok(new { exists = result.Exists });
        }

        [HttpPost("create")]
        public async Task<IActionResult> CreateUser([FromBody] UserService.CreateUserDTO dto)
        {
            var response = await _service.CreateUserAsync(dto);

            if (!response.Success)
                return BadRequest(new { error = response.Error });

            return Ok(new { message = "User created. Activation email sent." });
        }

        [HttpPut("{email}/role")]
        public async Task<IActionResult> UpdateRole(string email, [FromBody] UserService.UpdateRoleDTO dto)
        {
            var result = await _service.UpdateRoleAsync(email, dto);

            if (!result.Success)
                return BadRequest(new { error = result.Error });

            return Ok(new { message = "Role updated and activation email sent." });
        }

        [AllowAnonymous]
        [HttpGet("activate")]
        public async Task<IActionResult> ActivateByToken([FromQuery] string token)
        {
            var url = await _service.ActivateUserAsync(token);
            return Redirect(url);
        }

        [HttpPut("{email}/deactivate")]
        public async Task<IActionResult> Deactivate(string email)
        {
            var result = await _service.DeactivateUserAsync(email);

            if (!result.Success)
                return NotFound(new { error = result.Error });

            return Ok(new { message = "User deactivated" });
        }

        [HttpGet("get")]
        public async Task<IActionResult> GetAllUsers()
        {
            var list = await _service.GetAllUsersAsync();
            return Ok(list);
        }
        
        [HttpGet("get/{email}")]
        public async Task<IActionResult> GetUserByEmail(string email)
        {
            var user = await _service.GetUserByIdAsync(email);
            return Ok(user);
        }
    }
}
