using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Authentication;
using DDDSample1.Domain.PhysicalResources;
using DDDSample1.Domain.PhysicalResources.DTOs;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Controllers
{
    [ApiController]
    [Route("api/[controller]")]
    public class PhysicalResourcesController : ControllerBase
    {
        private readonly PhysicalResourceService _service;

        public PhysicalResourcesController(PhysicalResourceService service)
        {
            _service = service;
        }

        [HttpPost]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<PhysicalResourceDto>> Create([FromBody] CreatePhysicalResourceDto dto)
        {
            try
            {
                var result = await _service.CreateAsync(dto);
                return CreatedAtAction(nameof(GetById), new { id = result.Id }, result);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        [HttpPut("{id}")]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<PhysicalResourceDto>> Update(Guid id, [FromBody] UpdatePhysicalResourceDto dto)
        {
            try
            {
                var result = await _service.UpdateAsync(id, dto);
                return Ok(result);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        [HttpPatch("{id}/status")]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<PhysicalResourceDto>> ChangeStatus(Guid id, [FromBody] ChangeStatusDto dto)
        {
            try
            {
                var result = await _service.ChangeStatusAsync(id, dto.NewStatus);
                return Ok(result);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        [HttpGet("{id}")]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<PhysicalResourceDto>> GetById(Guid id)
        {
            var results = await _service.SearchAsync(null, null, null);
            var resource = results.FirstOrDefault(r => r.Id == id);
            if (resource == null)
                return NotFound();

            return Ok(resource);
        }

        [HttpGet]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<List<PhysicalResourceDto>>> Search(
            [FromQuery] string? description,
            [FromQuery] string? type,
            [FromQuery] ResourceStatus? status)
        {
            var results = await _service.SearchAsync(description, type, status);
            return Ok(results);
        }
    }
}
