
using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Authentication;
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
        //[AuthorizeRole(Roles.Admin)]
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
        //[AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<OrganizationDto>> GetById(string id)
        {
            try
            {
                var org = await _service.GetByIdAsync(id);

                if (org == null)
                    return NotFound(new { Message = $"Organization with ID {id} not found." });

                return Ok(org);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // POST: api/Organizations
        /// <summary>
        /// Regista uma nova organização.
        /// </summary>
        [HttpPost]
        //[AuthorizeRole(Roles.Admin)]
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
        
        
        // GET: api/Organizations/check-legalname/{name}
        [HttpGet("check-legalname/{name}")]
        public async Task<ActionResult<bool>> CheckLegalName(string name)
        {
            var exists = await _service.LegalNameExistsAsync(name);
            return Ok(exists);
        }
        [HttpGet("check-taxnumber")]
        public async Task<ActionResult<bool>> CheckTaxNumber(string taxNumber)
        {
            var exists = await _service.TaxNumberExistsAsync(taxNumber);
            return Ok(exists);
        }
    }
}
