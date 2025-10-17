using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Organizations;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class OrganizationsController : ControllerBase
    {
        private readonly OrganizationService _service;

        public OrganizationsController(OrganizationService service)
        {
            _service = service;
        }

        // GET: api/Organizations
        /// <summary>
        /// Obtém todas as organizações registradas.
        /// </summary>
        [HttpGet]
        public async Task<ActionResult<List<OrganizationDto>>> GetAll()
        {
            var orgs = await _service.GetAllAsync();
            return Ok(orgs);
        }

        // GET: api/Organizations/{id}
        /// <summary>
        /// Obtém uma organização específica por ID.
        /// </summary>
        [HttpGet("{id}")]
        public async Task<ActionResult<OrganizationDto>> GetById(Guid id)
        {
            var org = await _service.GetByIdAsync(id);

            if (org == null)
                return NotFound(new { Message = $"Organization with ID {id} not found." });

            return Ok(org);
        }

        // POST: api/Organizations/register
        /// <summary>
        /// Regista uma nova organização.
        /// </summary>
        [HttpPost("register")]
        public async Task<ActionResult<OrganizationDto>> Register([FromBody] OrganizationDto dto)
        {
            try
            {
                var org = await _service.RegisterOrganizationAsync(dto);
                return CreatedAtAction(nameof(GetById), new { id = org.Id }, org);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
    }
}
