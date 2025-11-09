// Controllers/AuthController.cs

using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Authentication;
using Microsoft.AspNetCore.Mvc;

namespace DDDSample1.Controllers
{
    [ApiController]
    [Route("auth/google")]
    public class AuthController : ControllerBase
    {
        private readonly GoogleAuthService _googleAuthService;

        public AuthController(GoogleAuthService googleAuthService)
        {
            _googleAuthService = googleAuthService;
        }

        [HttpPost]
        public async Task<IActionResult> ExchangeCode([FromBody] AuthCodeRequest request)
        {
            if (string.IsNullOrEmpty(request.Code))
                return BadRequest(new { error = "Code is required" });

            try
            {
                // Chama o serviço que troca o code pelo token
                var tokenData = await _googleAuthService.ExchangeCodeForToken(request.Code);
                return Ok(tokenData);
            }
            catch
            {
                return StatusCode(500, new { error = "Error changing code to token" });
            }
        }
        [HttpPost("user")]
        public async Task<IActionResult> GetUser([FromBody] TokenRequest request)
        {
            if (string.IsNullOrEmpty(request.IdToken))
                return BadRequest(new { error = "IdToken is required" });

            try
            {
                var tokenResponse = new TokenResponse
                {
                    IdToken = request.IdToken
                };

                var user = await _googleAuthService.GetUser(tokenResponse);
                
                
                if (user.GetRole() == Roles.NoRole)
                    return Unauthorized(new { error = "User has no assigned role. Access denied." });

                if (user.GetStatus() == Status.Inactive)
                    return Unauthorized(new { error = $"User account is {user.GetStatus()}. Access denied."});
                
                return Ok(new
                {
                    email = user.GetEmail().AsString(),
                    name = user.GetName(),
                    foto = user.GetPicture(),
                    role = user.GetRole().ToString(),
                    status = user.GetStatus().ToString()
                });
            }
            catch (Exception ex)
            {
                return NotFound(new { error = "User not found" });
            }
        }

    }
    
    

    public class AuthCodeRequest
    {
        public string Code { get; set; } = "";
    }
    public class TokenRequest
    {
        public string IdToken { get; set; } = "";
    }
}