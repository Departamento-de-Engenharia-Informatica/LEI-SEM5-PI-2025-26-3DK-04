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
    public class VesselTypesController : ControllerBase
    {
        private readonly VesselTypeService _service;

        public VesselTypesController(VesselTypeService service)
        {
            _service = service;
        }

        // GET: api/VesselTypes
        [HttpGet]
        public async Task<ActionResult<IEnumerable<VesselTypeDto>>> GetAll([FromQuery] string search)
        {
            if (!string.IsNullOrWhiteSpace(search))
            {
                return await _service.SearchAsync(search);
            }
            return await _service.GetAllAsync();
        }

        // GET: api/VesselTypes/5
        [HttpGet("{id}")]
        public async Task<ActionResult<VesselTypeDto>> GetById(Guid id)
        {
            var vesselType = await _service.GetByIdAsync(new VesselTypeId(id));

            if (vesselType == null)
            {
                return NotFound();
            }

            return vesselType;
        }

        // GET: api/VesselTypes/search/name?term=cargo
        [HttpGet("search/name")]
        public async Task<ActionResult<IEnumerable<VesselTypeDto>>> SearchByName([FromQuery] string term)
        {
            if (string.IsNullOrWhiteSpace(term))
            {
                return BadRequest(new { Message = "Search term is required." });
            }

            return await _service.SearchByNameAsync(term);
        }

        // GET: api/VesselTypes/search/description?term=container
        [HttpGet("search/description")]
        public async Task<ActionResult<IEnumerable<VesselTypeDto>>> SearchByDescription([FromQuery] string term)
        {
            if (string.IsNullOrWhiteSpace(term))
            {
                return BadRequest(new { Message = "Search term is required." });
            }

            return await _service.SearchByDescriptionAsync(term);
        }

        // POST: api/VesselTypes
        [HttpPost]
        public async Task<ActionResult<VesselTypeDto>> Create(CreatingVesselTypeDto dto)
        {
            try
            {
                var vesselType = await _service.AddAsync(dto);
                return CreatedAtAction(nameof(GetById), new { id = vesselType.Id }, vesselType);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PUT: api/VesselTypes/5
        [HttpPut("{id}")]
        public async Task<ActionResult<VesselTypeDto>> Update(Guid id, UpdatingVesselTypeDto dto)
        {
            try
            {
                var vesselType = await _service.UpdateAsync(id, dto);

                if (vesselType == null)
                {
                    return NotFound();
                }

                return Ok(vesselType);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // DELETE: api/VesselTypes/5 (Soft Delete - Inactivate)
        [HttpDelete("{id}")]
        public async Task<ActionResult<VesselTypeDto>> SoftDelete(Guid id)
        {
            try
            {
                var vesselType = await _service.InactivateAsync(new VesselTypeId(id));

                if (vesselType == null)
                {
                    return NotFound();
                }

                return Ok(vesselType);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // POST: api/VesselTypes/5/activate
        [HttpPost("{id}/activate")]
        public async Task<ActionResult<VesselTypeDto>> Activate(Guid id)
        {
            try
            {
                var vesselType = await _service.ActivateAsync(new VesselTypeId(id));

                if (vesselType == null)
                {
                    return NotFound();
                }

                return Ok(vesselType);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // DELETE: api/VesselTypes/5/hard (Hard Delete)
        [HttpDelete("{id}/hard")]
        public async Task<ActionResult<VesselTypeDto>> HardDelete(Guid id)
        {
            try
            {
                var vesselType = await _service.DeleteAsync(new VesselTypeId(id));

                if (vesselType == null)
                {
                    return NotFound();
                }

                return Ok(vesselType);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
    }
}
