// File: Controllers/VesselVisitNotificationsController.cs
using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Vessels;
using DDDSample1.Domain.Vessels.VesselVisitNotification;

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

        /// <summary>
        /// Cria uma nova notificação de visita de navio com dados de carga e, opcionalmente, tripulação.
        /// </summary>
        [HttpPost]
        public async Task<ActionResult<VesselVisitNotificationDto>> Create([FromBody] CreateNotificationDto dto)
        {
            try
            {
                var result = await _service.CreateAsync(dto);
                return CreatedAtAction(nameof(GetById), new { id = result.Id }, result);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        [HttpGet("completed")]
        public async Task<ActionResult<List<VesselVisitNotificationDto>>> GetCompletedNotifications()
        {
            var notifications = await _service.GetCompletedNotificationsAsync();
            return Ok(notifications);
        }

        [HttpGet("{id}")]
        public async Task<ActionResult<VesselVisitNotificationDto>> GetById(Guid id)
        {
            var notification = await _service.GetByIdAsync(id);
            if (notification == null)
                return NotFound(new { Message = $"Notification with ID {id} not found." });

            return Ok(notification);
        }

        [HttpPut("{id}/approve")]
        public async Task<ActionResult<VesselVisitNotificationDto>> Approve(Guid id, [FromBody] ApproveNotificationDto dto)
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

        [HttpPut("{id}/reject")]
        public async Task<ActionResult<VesselVisitNotificationDto>> Reject(Guid id, [FromBody] RejectNotificationDto dto)
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

        [HttpPut("{id}/update")]
        public async Task<ActionResult<VesselVisitNotificationDto>> UpdateInProgress(string id, [FromBody] UpdateNotificationDto dto)
        {
            try
            {
                var updated = await _service.UpdateInProgressAsync(id, dto);
                return Ok(updated);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        [HttpPut("{id}/submit")]
        public async Task<ActionResult<VesselVisitNotificationDto>> Submit(Guid id)
        {
            try
            {
                var submitted = await _service.SubmitForApprovalAsync(id);
                return Ok(submitted);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
        [HttpPut("{id}/withdraw")]
        public async Task<ActionResult<VesselVisitNotificationDto>> Withdraw(Guid id)
        {
            try
            {
                var notification = await _service.WithdrawRequestAsync(id);
                return Ok(notification);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        [HttpPut("{id}/resume")]
        public async Task<ActionResult<VesselVisitNotificationDto>> Resume(Guid id)
        {
            try
            {
                var notification = await _service.ResumeAsync(id);
                return Ok(notification);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
        
        // PUT: api/VesselVisitNotifications/{id}/reset
        /// <summary>
        /// Reseta uma notificação rejeitada para o estado "InProgress" para permitir edição.
        /// </summary>
        [HttpPut("{id}/reset")]
        public async Task<ActionResult<VesselVisitNotificationDto>> ResetToInProgress(Guid id)
        {
            try
            {
                var reset = await _service.ResetToInProgressAsync(id);
                return Ok(reset);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
        
        /// <summary>
        /// US 2.2.10: Search and filter vessel visit notifications
        /// Representatives can view their own notifications and those from colleagues in the same organization
        /// </summary>
        [HttpGet("search")]
        public async Task<ActionResult<List<VesselVisitNotificationDto>>> SearchNotifications(
            [FromQuery] Guid? vesselId = null,
            [FromQuery] string status = null,
            [FromQuery] string? representativeId = null,
            [FromQuery] Guid? organizationId = null,
            [FromQuery] DateTime? startDate = null,
            [FromQuery] DateTime? endDate = null)
        {
            try
            {
                var filter = new NotificationFilterDto
                {
                    VesselId = vesselId,
                    Status = string.IsNullOrWhiteSpace(status) ? null : Enum.Parse<NotificationStatus>(status, true),
                    RepresentativeId = representativeId,
                    OrganizationId = organizationId,
                    StartDate = startDate,
                    EndDate = endDate
                };
                
                var notifications = await _service.SearchNotificationsAsync(filter);
                return Ok(notifications);
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { Message = $"Invalid status value. {ex.Message}" });
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
    }
}