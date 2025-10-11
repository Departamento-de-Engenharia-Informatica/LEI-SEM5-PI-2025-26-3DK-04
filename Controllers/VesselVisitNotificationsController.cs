using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Vessels;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class VesselVisitNotificationsController : ControllerBase
    {
        private readonly VesselVisitNotificationService _service;
        
        public VesselVisitNotificationsController(VesselVisitNotificationService service)
        {
            _service = service;
        }
        
        // GET: api/VesselVisitNotifications/completed
        /// <summary>
        /// Obtém todas as notificações completadas e prontas para revisão
        /// </summary>
        [HttpGet("completed")]
        public async Task<ActionResult<List<VesselVisitNotificationDto>>> GetCompletedNotifications()
        {
            var notifications = await _service.GetCompletedNotificationsAsync();
            return Ok(notifications);
        }
        
        // GET: api/VesselVisitNotifications/{id}
        /// <summary>
        /// Obtém uma notificação específica por ID
        /// </summary>
        [HttpGet("{id}")]
        public async Task<ActionResult<VesselVisitNotificationDto>> GetById(Guid id)
        {
            var notification = await _service.GetByIdAsync(id);
            
            if (notification == null)
                return NotFound(new { Message = $"Notification with ID {id} not found." });
            
            return Ok(notification);
        }
        
        // PUT: api/VesselVisitNotifications/{id}/approve
        /// <summary>
        /// Aprova uma notificação completada e atribui um dock
        /// </summary>
        [HttpPut("{id}/approve")]
        public async Task<ActionResult<VesselVisitNotificationDto>> Approve(
            Guid id,
            [FromBody] ApproveNotificationDto dto)
        {
            try
            {
                var notification = await _service.ApproveAsync(id, dto.DockId, dto.OfficerId);
                return Ok(notification);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
        
        // PUT: api/VesselVisitNotifications/{id}/reject
        /// <summary>
        /// Rejeita uma notificação completada com um motivo
        /// </summary>
        [HttpPut("{id}/reject")]
        public async Task<ActionResult<VesselVisitNotificationDto>> Reject(
            Guid id,
            [FromBody] RejectNotificationDto dto)
        {
            try
            {
                var notification = await _service.RejectAsync(id, dto.Reason, dto.OfficerId);
                return Ok(notification);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
    }
}
