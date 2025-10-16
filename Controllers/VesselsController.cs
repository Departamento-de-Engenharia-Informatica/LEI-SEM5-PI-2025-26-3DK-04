using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Vessels;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class VesselsController : ControllerBase
    {
        private readonly VesselService _service;

        public VesselsController(VesselService service)
        {
            _service = service;
        }

        // GET: api/Vessels
        [HttpGet]
        public async Task<ActionResult<IEnumerable<VesselDto>>> GetAll([FromQuery] string search)
        {
            if (!string.IsNullOrWhiteSpace(search))
            {
                return await _service.SearchAsync(search);
            }
            return await _service.GetAllAsync();
        }

        // GET: api/Vessels/5
        [HttpGet("{id}")]
        public async Task<ActionResult<VesselDto>> GetById(Guid id)
        {
            var vessel = await _service.GetByIdAsync(new VesselId(id));

            if (vessel == null)
            {
                return NotFound();
            }

            return vessel;
        }

        // GET: api/Vessels/imo/1234567
        [HttpGet("imo/{imoNumber}")]
        public async Task<ActionResult<VesselDto>> GetByImoNumber(string imoNumber)
        {
            try
            {
                var vessel = await _service.GetByImoNumberAsync(imoNumber);

                if (vessel == null)
                {
                    return NotFound();
                }

                return vessel;
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // GET: api/Vessels/search/name?term=cargo
        [HttpGet("search/name")]
        public async Task<ActionResult<IEnumerable<VesselDto>>> SearchByName([FromQuery] string term)
        {
            if (string.IsNullOrWhiteSpace(term))
            {
                return BadRequest(new { Message = "Search term is required." });
            }

            return await _service.SearchByNameAsync(term);
        }

        // GET: api/Vessels/search/owner?term=company
        [HttpGet("search/owner")]
        public async Task<ActionResult<IEnumerable<VesselDto>>> SearchByOwner([FromQuery] string term)
        {
            if (string.IsNullOrWhiteSpace(term))
            {
                return BadRequest(new { Message = "Search term is required." });
            }

            return await _service.SearchByOwnerAsync(term);
        }

        // GET: api/Vessels/search/operator?term=maersk
        [HttpGet("search/operator")]
        public async Task<ActionResult<IEnumerable<VesselDto>>> SearchByOperator([FromQuery] string term)
        {
            if (string.IsNullOrWhiteSpace(term))
            {
                return BadRequest(new { Message = "Search term is required." });
            }

            return await _service.SearchByOperatorAsync(term);
        }

        // POST: api/Vessels
        [HttpPost]
        public async Task<ActionResult<VesselDto>> Create(CreatingVesselDto dto)
        {
            try
            {
                var vessel = await _service.AddAsync(dto);
                return CreatedAtAction(nameof(GetById), new { id = vessel.Id }, vessel);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PUT: api/Vessels/5
        [HttpPut("{id}")]
        public async Task<ActionResult<VesselDto>> Update(Guid id, UpdatingVesselDto dto)
        {
            try
            {
                var vessel = await _service.UpdateAsync(id, dto);

                if (vessel == null)
                {
                    return NotFound();
                }

                return Ok(vessel);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // DELETE: api/Vessels/5 (Soft Delete - Inactivate)
        [HttpDelete("{id}")]
        public async Task<ActionResult<VesselDto>> SoftDelete(Guid id)
        {
            try
            {
                var vessel = await _service.InactivateAsync(new VesselId(id));

                if (vessel == null)
                {
                    return NotFound();
                }

                return Ok(vessel);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // POST: api/Vessels/5/activate
        [HttpPost("{id}/activate")]
        public async Task<ActionResult<VesselDto>> Activate(Guid id)
        {
            try
            {
                var vessel = await _service.ActivateAsync(new VesselId(id));

                if (vessel == null)
                {
                    return NotFound();
                }

                return Ok(vessel);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // DELETE: api/Vessels/5/hard (Hard Delete)
        [HttpDelete("{id}/hard")]
        public async Task<ActionResult<VesselDto>> HardDelete(Guid id)
        {
            try
            {
                var vessel = await _service.DeleteAsync(new VesselId(id));

                if (vessel == null)
                {
                    return NotFound();
                }

                return Ok(vessel);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
    }
}
