using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels
{
    public class VesselVisitNotificationService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IVesselVisitNotificationRepository _repo;
        
        public VesselVisitNotificationService(
            IUnitOfWork unitOfWork,
            IVesselVisitNotificationRepository repo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
        }
        
        // Listar notificações completadas (prontas para review)
        public async Task<List<VesselVisitNotificationDto>> GetCompletedNotificationsAsync()
        {
            var notifications = await _repo.GetCompletedNotificationsAsync();
            
            return notifications.Select(n => new VesselVisitNotificationDto
            {
                Id = n.Id.AsGuid(),
                State = n.State,
                AssignedDock = n.AssignedDock,
                RejectedReason = n.RejectedReason,
                DecisionTimeStamp = n.DecisionTimeStamp,
                DecisionOutcome = n.DecisionOutcome,
                OfficerId = n.OfficerId,
                LoadingCargo = n.LoadingCargo,
                UnloadingCargo = n.UnloadingCargo
            }).ToList();
        }
        
        // Procurar notificação por ID
        public async Task<VesselVisitNotificationDto> GetByIdAsync(Guid id)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));
            
            if (notification == null)
                return null;
            
            return new VesselVisitNotificationDto
            {
                Id = notification.Id.AsGuid(),
                State = notification.State,
                AssignedDock = notification.AssignedDock,
                RejectedReason = notification.RejectedReason,
                DecisionTimeStamp = notification.DecisionTimeStamp,
                DecisionOutcome = notification.DecisionOutcome,
                OfficerId = notification.OfficerId,
                LoadingCargo = notification.LoadingCargo,
                UnloadingCargo = notification.UnloadingCargo
            };
        }
        
        // Aprovar notificação
        public async Task<VesselVisitNotificationDto> ApproveAsync(Guid id, string dockId, string officerId)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));
            
            if (notification == null)
                throw new BusinessRuleValidationException("Vessel Visit Notification not found.");
            
            // Chamar método de domínio que contém as regras de negócio
            notification.Approve(dockId, officerId);
            
            // Persistir mudanças
            await _unitOfWork.CommitAsync();
            
            return new VesselVisitNotificationDto
            {
                Id = notification.Id.AsGuid(),
                State = notification.State,
                AssignedDock = notification.AssignedDock,
                RejectedReason = notification.RejectedReason,
                DecisionTimeStamp = notification.DecisionTimeStamp,
                DecisionOutcome = notification.DecisionOutcome,
                OfficerId = notification.OfficerId,
                LoadingCargo = notification.LoadingCargo,
                UnloadingCargo = notification.UnloadingCargo
            };
        }
        
        // Rejeitar notificação
        public async Task<VesselVisitNotificationDto> RejectAsync(Guid id, string reason, string officerId)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));
            
            if (notification == null)
                throw new BusinessRuleValidationException("Vessel Visit Notification not found.");
            
            // Chamar método de domínio que contém as regras de negócio
            notification.Reject(reason, officerId);
            
            // Persistir mudanças
            await _unitOfWork.CommitAsync();
            
            return new VesselVisitNotificationDto
            {
                Id = notification.Id.AsGuid(),
                State = notification.State,
                AssignedDock = notification.AssignedDock,
                RejectedReason = notification.RejectedReason,
                DecisionTimeStamp = notification.DecisionTimeStamp,
                DecisionOutcome = notification.DecisionOutcome,
                OfficerId = notification.OfficerId,
                LoadingCargo = notification.LoadingCargo,
                UnloadingCargo = notification.UnloadingCargo
            };
        }
    }
}
