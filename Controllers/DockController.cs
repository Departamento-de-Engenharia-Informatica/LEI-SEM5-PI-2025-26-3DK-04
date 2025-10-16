using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Shared;
using DDDNetCore.Infraestructure.Docks;
using DDDSample1.Infrastructure.Docks;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class DockController : ControllerBase
    {
        private readonly DockService _service;

        public DockController(DockService service)
        {
            _service = service;
        }

        // GET: api/Dock
        [HttpGet]
        public async Task<ActionResult<IEnumerable<DockDetailsDto>>> GetAll()
        {
            return await _service.GetAllAsync();
        }

        // GET: api/Dock/{id}
        [HttpGet("{id}")]
        public async Task<ActionResult<DockDetailsDto>> GetById(string id)
        {
            var dock = await _service.GetByIdAsync(new DockID(id));

            if (dock == null)
            {
                return NotFound();
            }

            return dock;
        }

        // POST: api/Dock
        [HttpPost]
        public async Task<ActionResult<DockDetailsDto>> Create(DockDto dto)
        {
            var dock = await _service.AddAsync(dto);

            return CreatedAtAction(nameof(GetById), new { id = dock.Id }, dock);
        }

        // PUT: api/Dock/{id}
        [HttpPut("{id}")]
        public async Task<ActionResult<DockDetailsDto>> Update(string id, DockDto dto)
        {
            try
            {
                var dock = await _service.UpdateAsync(id, dto);

                if (dock == null)
                {
                    return NotFound();
                }

                return Ok(dock);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // DELETE: api/Dock/{id}
        [HttpDelete("{id}")]
        public async Task<ActionResult<DockDetailsDto>> SoftDelete(string id)
        {
            var dock = await _service.InactivateAsync(new DockID(id));

            if (dock == null)
            {
                return NotFound();
            }

            return Ok(dock);
        }

        // DELETE: api/Dock/{id}/hard
        [HttpDelete("{id}/hard")]
        public async Task<ActionResult<DockDetailsDto>> HardDelete(string id)
        {
            try
            {
                var dock = await _service.DeleteAsync(new DockID(id));

                if (dock == null)
                {
                    return NotFound();
                }

                return Ok(dock);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
    }
}
