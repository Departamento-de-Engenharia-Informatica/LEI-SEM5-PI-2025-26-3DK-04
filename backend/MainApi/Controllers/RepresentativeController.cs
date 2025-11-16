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
    public class RepresentativesController : ControllerBase
    {
        private readonly RepresentativeService _service;

        public RepresentativesController(RepresentativeService service)
        {
            _service = service;
        }

        // POST: api/Representatives
        [HttpPost]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<RepresentativeDto>> AddRepresentative([FromBody] AddRepresentativeDto dto)
        {
            try
            {
                var rep = await _service.AddRepresentativeAsync(dto);
                return Ok(rep);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PUT: api/Representatives/{id}/update
        [HttpPut("{id}/update")]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<RepresentativeDto>> UpdateRepresentative([FromRoute] string id, [FromBody] UpdateRepresentativeDto dto)
        {
            try
            {
                var rep = await _service.UpdateRepresentativeAsync(id, dto);
                return Ok(rep);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PUT: api/Representatives/{id}/deactivate
        [HttpPut("{id}/deactivate")]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<RepresentativeDto>> DeactivateRepresentative(string id)
        {
            try
            {
                var rep = await _service.DeactivateRepresentativeAsync(id);
                return Ok(rep);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PUT: api/Representatives/{id}/activate
        [HttpPut("{id}/activate")]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<RepresentativeDto>> ActivateRepresentative(string id)
        {
            try
            {
                var rep = await _service.ActivateRepresentativeAsync(id);
                return Ok(rep);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // GET: api/Representatives
        [HttpGet]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<List<RepresentativeDto>>> GetAll()
        {
            var reps = await _service.GetAllAsync();
            return Ok(reps);
        }

        // GET: api/Representatives/{id}
        [HttpGet("{id}")]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<RepresentativeDto>> GetById(string id)
        {
            var rep = await _service.GetByIdAsync(id);

            return Ok(rep);
        }
        
        [HttpGet("active")]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<IEnumerable<RepresentativeDto>>> GetActiveRepresentatives()
        {
            var reps = await _service.GetActiveAsync();
            return Ok(reps);
        }

        [HttpGet("inactive")]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<IEnumerable<RepresentativeDto>>> GetInactiveRepresentatives()
        {
            var reps = await _service.GetInactiveAsync();
            return Ok(reps);
        }
        
        [HttpGet("check-email/{email}")]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<bool>> CheckEmail(string email)
        {
            var exists = await _service.EmailExistsAsync(email);
            return Ok(exists);
        }

        [HttpGet("check-citizenid/{cid}")]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<bool>> CheckCitizenId(string cid)
        {
            var exists = await _service.CitizenIdExistsAsync(cid);
            return Ok(exists);
        }
        
        [HttpGet("check-phone/{phone}")]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<bool>> CheckPhone(string phone)
        {
            var exists = await _service.PhoneExistsAsync(phone);
            return Ok(exists);
        }
    }
}
