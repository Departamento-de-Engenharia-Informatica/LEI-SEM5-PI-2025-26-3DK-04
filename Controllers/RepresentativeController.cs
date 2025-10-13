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
    public class RepresentativesController : ControllerBase
    {
        private readonly RepresentativeService _service;

        public RepresentativesController(RepresentativeService service)
        {
            _service = service;
        }

        // POST: api/Representatives
        /// <summary>
        /// Cria um novo representante
        /// </summary>
        [HttpPost]
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

        // PUT: api/Representatives/{id}
        /// <summary>
        /// Atualiza os dados de um representante existente
        /// </summary>
        [HttpPut("{id}")]
        public async Task<ActionResult<RepresentativeDto>> UpdateRepresentative(Guid id, [FromBody] AddRepresentativeDto dto)
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
        /// <summary>
        /// Desativa um representante
        /// </summary>
        [HttpPut("{id}/deactivate")]
        public async Task<ActionResult<RepresentativeDto>> DeactivateRepresentative(Guid id)
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
        /// <summary>
        /// Ativa um representante previamente desativado
        /// </summary>
        [HttpPut("{id}/activate")]
        public async Task<ActionResult<RepresentativeDto>> ActivateRepresentative(Guid id)
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
        /// <summary>
        /// Obtém todos os representantes
        /// </summary>
        [HttpGet]
        public async Task<ActionResult<List<RepresentativeDto>>> GetAll()
        {
            var reps = await _service.GetAllAsync();
            return Ok(reps);
        }

        // GET: api/Representatives/{id}
        /// <summary>
        /// Obtém um representante específico por ID
        /// </summary>
        [HttpGet("{id}")]
        public async Task<ActionResult<RepresentativeDto>> GetById(Guid id)
        {
            var rep = await _service.GetByIdAsync(id);
            if (rep == null)
                return NotFound(new { Message = $"Representative with ID {id} not found." });

            return Ok(rep);
        }
    }
}
