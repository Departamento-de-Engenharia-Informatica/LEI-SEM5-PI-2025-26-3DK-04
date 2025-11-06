using System;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using IartiApi.Services;
using IartiApi.DTOs;

namespace IartiApi.Controllers
{
    /// <summary>
    /// Controller for planning and scheduling algorithms
    /// </summary>
    [ApiController]
    [Route("api/[controller]")]
    public class PlanningController : ControllerBase
    {
        private readonly PrologService _prologService;

        public PlanningController(PrologService prologService)
        {
            _prologService = prologService;
        }

        /// <summary>
        /// Tests the connection to the Prolog planning server
        /// </summary>
        /// <returns>Connection status message</returns>
        /// <response code="200">Connection successful</response>
        /// <response code="500">Internal server error or Prolog server unavailable</response>
        [HttpGet("test")]
        [ProducesResponseType(typeof(object), 200)]
        [ProducesResponseType(typeof(object), 500)]
        public async Task<IActionResult> TestConnection()
        {
            try
            {
                var result = await _prologService.TestConnectionAsync();
                return Ok(new { message = result, status = "success" });
            }
            catch (Exception ex)
            {
                return StatusCode(500, new { error = ex.Message, status = "error" });
            }
        }

        /// <summary>
        /// Registers a user through the planning system (example endpoint)
        /// </summary>
        /// <param name="dto">User registration data</param>
        /// <returns>Registration confirmation</returns>
        /// <response code="200">User registered successfully</response>
        /// <response code="400">Invalid input data</response>
        /// <response code="500">Internal server error</response>
        [HttpPost("register")]
        [ProducesResponseType(typeof(object), 200)]
        [ProducesResponseType(typeof(object), 400)]
        [ProducesResponseType(typeof(object), 500)]
        public async Task<IActionResult> RegisterUser([FromBody] UserRegistrationDto dto)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            try
            {
                var result = await _prologService.RegisterUserAsync(dto.Name, dto.Sex, dto.BirthYear);
                return Ok(new { message = result, status = "success" });
            }
            catch (Exception ex)
            {
                return StatusCode(500, new { error = ex.Message, status = "error" });
            }
        }

        /// <summary>
        /// Sends a file to the planning system for processing (example endpoint)
        /// </summary>
        /// <param name="dto">File upload data</param>
        /// <returns>File processing result</returns>
        /// <response code="200">File processed successfully</response>
        /// <response code="400">Invalid input data</response>
        /// <response code="500">Internal server error</response>
        [HttpPost("upload")]
        [ProducesResponseType(typeof(object), 200)]
        [ProducesResponseType(typeof(object), 400)]
        [ProducesResponseType(typeof(object), 500)]
        public async Task<IActionResult> UploadFile([FromBody] FileUploadDto dto)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            try
            {
                var result = await _prologService.SendFileAsync(dto.FileContent);
                return Ok(new { message = result, status = "success" });
            }
            catch (Exception ex)
            {
                return StatusCode(500, new { error = ex.Message, status = "error" });
            }
        }
    }
}
