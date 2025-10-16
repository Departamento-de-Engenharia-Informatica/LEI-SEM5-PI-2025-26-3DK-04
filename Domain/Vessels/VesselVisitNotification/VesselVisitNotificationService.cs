using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Vessels.VesselInformation;

namespace DDDSample1.Domain.Vessels.VesselVisitNotification
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
        
        //Create new notification
        public async Task<VesselVisitNotificationDto> CreateAsync(
            List<CargoManifest> loadingManifests,
            List<CargoManifest> unloadingManifests,
            List<CrewMember> crewMembers)
        {
            var loadingCargo = loadingManifests != null && loadingManifests.Any()
                ? new LoadingCargoMaterial(loadingManifests)
                : null;

            var unloadingCargo = unloadingManifests != null && unloadingManifests.Any()
                ? new UnloadingCargoMaterial(unloadingManifests)
                : null;

            var notification = new VesselVisitNotification(loadingCargo, unloadingCargo);

            await _repo.AddAsync(notification);
            await _unitOfWork.CommitAsync();

            return MapToDto(notification);
        }
        
        // Listar notificações completadas (prontas para review)
        public async Task<List<VesselVisitNotificationDto>> GetCompletedNotificationsAsync()
        {
            var notifications = await _repo.GetCompletedNotificationsAsync();
            
            return notifications.Select(n => MapToDto(n)).ToList();
        }
        
        // Procurar notificação por ID
        public async Task<VesselVisitNotificationDto> GetByIdAsync(Guid id)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));
            
            if (notification == null)
                return null;
            
            return MapToDto(notification);
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
            
            return MapToDto(notification);
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

            return MapToDto(notification);
        }
        
        // Helper method
        private VesselVisitNotificationDto MapToDto(VesselVisitNotification notification)
        {
            var dto = new VesselVisitNotificationDto(notification.LoadingCargo, notification.UnloadingCargo)
            {
                Id = notification.Id.AsGuid(),
                Status = notification.Status,
                AssignedDock = null, // AssignedDock é string na entidade, mas Dock no DTO - precisa ser mapeado
                RejectedReason = notification.RejectedReason,
                DecisionTimeStamp = notification.DecisionTimeStamp,
                DecisionOutcome = notification.DecisionOutcome,
                OfficerId = notification.OfficerId
            };
            return dto;
        }
        
        public async Task<VesselVisitNotificationDto> UpdateInProgressAsync(Guid id, LoadingCargoMaterial newLoading, UnloadingCargoMaterial newUnloading)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));

            if (notification == null)
                throw new BusinessRuleValidationException("Vessel Visit Notification not found.");

            notification.UpdateInProgress(newLoading, newUnloading);

            await _unitOfWork.CommitAsync();
            return MapToDto(notification);
        }

        public async Task<VesselVisitNotificationDto> SubmitForApprovalAsync(Guid id)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));

            if (notification == null)
                throw new BusinessRuleValidationException("Vessel Visit Notification not found.");

            notification.SubmitForApproval();

            await _unitOfWork.CommitAsync();
            return MapToDto(notification);
        }

    }
}
