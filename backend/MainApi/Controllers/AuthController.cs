// Controllers/AuthController.cs
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
    }

    public class AuthCodeRequest
    {
        public string Code { get; set; } = "";
    }
}