using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StaffMembers;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class StaffMembersController : ControllerBase
    {
        private readonly StaffMemberService _service;
        
        public StaffMembersController(StaffMemberService service)
        {
            _service = service;
        }
        
        // POST: api/StaffMembers
        /// <summary>
        /// Cria um novo staff member
        /// </summary>
        [HttpPost]
        public async Task<ActionResult<StaffMemberDto>> Create([FromBody] CreateStaffMemberDto dto)
        {
            try
            {
                var staffMember = await _service.CreateAsync(dto);
                return CreatedAtAction(nameof(GetById), new { id = staffMember.Id }, staffMember);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
        
        // GET: api/StaffMembers/{id}
        /// <summary>
        /// Obtém um staff member específico por ID
        /// </summary>
        [HttpGet("{id}")]
        public async Task<ActionResult<StaffMemberDto>> GetById(Guid id)
        {
            var staffMember = await _service.GetByIdAsync(id);
            
            if (staffMember == null)
                return NotFound(new { Message = $"Staff member with ID {id} not found." });
            
            return Ok(staffMember);
        }
        
        // PUT: api/StaffMembers/{id}
        /// <summary>
        /// Atualiza um staff member existente (atualização parcial)
        /// </summary>
        [HttpPut("{id}")]
        public async Task<ActionResult<StaffMemberDto>> Update(
            Guid id,
            [FromBody] UpdateStaffMemberDto dto)
        {
            try
            {
                var staffMember = await _service.UpdateAsync(id, dto);
                return Ok(staffMember);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
        
        // DELETE: api/StaffMembers/{id}
        /// <summary>
        /// Desativa um staff member (soft delete - preserva dados para auditoria)
        /// </summary>
        [HttpDelete("{id}")]
        public async Task<ActionResult<StaffMemberDto>> Deactivate(Guid id)
        {
            try
            {
                var staffMember = await _service.DeactivateAsync(id);
                return Ok(staffMember);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
        
        // PUT: api/StaffMembers/{id}/reactivate
        /// <summary>
        /// Reativa um staff member previamente desativado
        /// </summary>
        [HttpPut("{id}/reactivate")]
        public async Task<ActionResult<StaffMemberDto>> Reactivate(Guid id)
        {
            try
            {
                var staffMember = await _service.ReactivateAsync(id);
                return Ok(staffMember);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
        
        // POST: api/StaffMembers/{id}/qualifications
        /// <summary>
        /// Adiciona uma qualificação a um staff member
        /// </summary>
        [HttpPost("{id}/qualifications")]
        public async Task<ActionResult<StaffMemberDto>> AddQualification(
            Guid id,
            [FromBody] AddQualificationDto dto)
        {
            try
            {
                var staffMember = await _service.AddQualificationAsync(id, dto.QualificationId);
                return Ok(staffMember);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
        
        // DELETE: api/StaffMembers/{staffId}/qualifications/{qualificationId}
        /// <summary>
        /// Remove uma qualificação de um staff member
        /// </summary>
        [HttpDelete("{staffId}/qualifications/{qualificationId}")]
        public async Task<ActionResult<StaffMemberDto>> RemoveQualification(
            Guid staffId,
            Guid qualificationId)
        {
            try
            {
                var staffMember = await _service.RemoveQualificationAsync(staffId, qualificationId);
                return Ok(staffMember);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
        
        // GET: api/StaffMembers
        /// <summary>
        /// Lista todos os staff members ativos
        /// </summary>
        [HttpGet]
        public async Task<ActionResult<List<StaffMemberDto>>> GetAllActive()
        {
            var staffMembers = await _service.GetAllActiveAsync();
            return Ok(staffMembers);
        }
        
        // GET: api/StaffMembers/all
        /// <summary>
        /// Lista todos os staff members
        /// </summary>
        [HttpGet("all")]
        public async Task<ActionResult<List<StaffMemberDto>>> GetAllForAudit()
        {
            var staffMembers = await _service.GetAllForAuditAsync();
            return Ok(staffMembers);
        }
        
        // GET: api/StaffMembers/search
        /// <summary>
        /// Pesquisa staff members com múltiplos filtros opcionais
        /// Query params: name, status, qualificationId
        /// </summary>
        [HttpGet("search")]
        public async Task<ActionResult<List<StaffMemberDto>>> Search(
            [FromQuery] string name = null,
            [FromQuery] MemberStatus? status = null,
            [FromQuery] Guid? qualificationId = null)
        {
            var staffMembers = await _service.SearchAsync(name, status, qualificationId);
            return Ok(staffMembers);
        }
        
        // GET: api/StaffMembers/by-name/{name}
        /// <summary>
        /// Pesquisa staff members por nome
        /// </summary>
        [HttpGet("by-name/{name}")]
        public async Task<ActionResult<List<StaffMemberDto>>> SearchByName(string name)
        {
            var staffMembers = await _service.SearchByNameAsync(name);
            return Ok(staffMembers);
        }
        
        // GET: api/StaffMembers/by-status/{status}
        /// <summary>
        /// Filtra staff members por status
        /// </summary>
        [HttpGet("by-status/{status}")]
        public async Task<ActionResult<List<StaffMemberDto>>> GetByStatus(MemberStatus status)
        {
            var staffMembers = await _service.GetByStatusAsync(status);
            return Ok(staffMembers);
        }
        
        // GET: api/StaffMembers/by-qualification/{qualificationId}
        /// <summary>
        /// Filtra staff members que têm uma qualificação específica
        /// </summary>
        [HttpGet("by-qualification/{qualificationId}")]
        public async Task<ActionResult<List<StaffMemberDto>>> GetByQualification(Guid qualificationId)
        {
            var staffMembers = await _service.GetByQualificationAsync(qualificationId);
            return Ok(staffMembers);
        }
    }
}
