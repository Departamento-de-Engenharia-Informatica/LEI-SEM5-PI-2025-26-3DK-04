using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Authentication;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Qualifications;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class QualificationsController : ControllerBase
    {
        private readonly QualificationService _service;

        public QualificationsController(QualificationService service)
        {
            _service = service;
        }
        
        // POST: api/Qualifications
        /// <summary>
        /// Cria uma nova qualificação
        /// </summary>
        [HttpPost]
        //[AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<QualificationDto>> Create([FromBody] CreateQualificationDto dto)
        {
            try
            {
                var qualification = await _service.CreateAsync(dto);
                return CreatedAtAction(nameof(GetById), new { id = qualification.Id }, qualification);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
        
        // GET: api/Qualifications/{id}
        /// <summary>
        /// Obtém uma qualificação específica por ID
        /// </summary>
        [HttpGet("{id}")]
        //[AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<QualificationDto>> GetById(Guid id)
        {
            var qualification = await _service.GetByIdAsync(id);
            
            if (qualification == null)
                return NotFound(new { Message = $"Qualification with ID {id} not found." });
            
            return Ok(qualification);
        }
        
        // GET: api/Qualifications
        /// <summary>
        /// Lista todas as qualificações
        /// </summary>
        [HttpGet]
        //[AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<List<QualificationDto>>> GetAll()
        {
            var qualifications = await _service.GetAllAsync();
            return Ok(qualifications);
        }
        
        // PUT: api/Qualifications/{id}
        /// <summary>
        /// Atualiza uma qualificação existente
        /// </summary>
        [HttpPut("{id}")]
        //[AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<QualificationDto>> Update(
            Guid id,
            [FromBody] UpdateQualificationDto dto)
        {
            try
            {
                var qualification = await _service.UpdateAsync(id, dto);
                return Ok(qualification);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // DELETE: api/Qualifications/{id}
        /// <summary>
        /// Remove uma qualificação
        /// </summary>
        [HttpDelete("{id}")]
        //[AuthorizeRole(Roles.Admin)]
        public async Task<IActionResult> Delete(Guid id)
        {
            try
            {
                await _service.DeleteAsync(id);
                return NoContent(); // 204 No Content
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
        
        // GET: api/Qualifications/search?name=...
        /// <summary>
        /// Pesquisa qualificações por nome
        /// </summary>
        [HttpGet("search")]
        //[AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<List<QualificationDto>>> SearchByName([FromQuery] string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                return BadRequest(new { Message = "Name parameter is required." });
            
            var qualifications = await _service.SearchByNameAsync(name);
            return Ok(qualifications);
        }
        
        // GET: api/Qualifications/exists/{id}
        /// <summary>
        /// Verifica se uma qualificação existe por ID
        /// </summary>
        [HttpGet("exists/{id}")]
        //[AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<bool>> ExistsById(Guid id)
        {
            var qualification = await _service.GetByIdAsync(id);
            return Ok(new { Exists = qualification != null });
        }
    }
}
