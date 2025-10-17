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
        private readonly IVesselRepository _vesselRepo;
        private readonly IVesselTypeRepository _vesselTypeRepo;
        
        public VesselVisitNotificationService(
            IUnitOfWork unitOfWork,
            IVesselVisitNotificationRepository repo,
            IVesselRepository vesselRepo,
            IVesselTypeRepository vesselTypeRepo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _vesselRepo = vesselRepo;
            _vesselTypeRepo = vesselTypeRepo;
        }
        
        //Create new notification
        public async Task<VesselVisitNotificationDto> CreateAsync(
            Guid vesselId,
            List<CargoManifest> loadingManifests,
            List<CargoManifest> unloadingManifests)
        {
            // Procurar o vessel
            var vessel = await _vesselRepo.GetByIdAsync(new VesselId(vesselId));
            if (vessel == null)
                throw new BusinessRuleValidationException("Vessel not found.");

            // Procurar o vessel type para obter a capacidade
            var vesselType = await _vesselTypeRepo.GetByIdAsync(vessel.VesselTypeId);
            if (vesselType == null)
                throw new BusinessRuleValidationException("Vessel type not found.");

            var loadingCargo = loadingManifests != null && loadingManifests.Any()
                ? new LoadingCargoMaterial(loadingManifests)
                : null;

            var unloadingCargo = unloadingManifests != null && unloadingManifests.Any()
                ? new UnloadingCargoMaterial(unloadingManifests)
                : null;

            // Validar que o unloading cargo não excede a capacidade do vessel
            if (unloadingCargo != null)
            {
                var unloadingWeight = unloadingCargo.TotalWeightKg();
                var vesselCapacity = vesselType.Capacity;
                
                if (unloadingWeight > vesselCapacity)
                    throw new BusinessRuleValidationException(
                        $"Unloading cargo weight ({unloadingWeight} kg) cannot exceed vessel capacity ({vesselCapacity} kg).");
            }

            var notification = new VesselVisitNotification(vessel, loadingCargo, unloadingCargo);

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
                OfficerId = notification.OfficerId,
                VesselId = notification.Vessel.Id.AsGuid(),
                VesselName = notification.Vessel.Name,
                VesselCallsign = notification.Vessel.ImoNumber.ToString()
            };
            return dto;
        }
        
        public async Task<VesselVisitNotificationDto> UpdateInProgressAsync(Guid id, LoadingCargoMaterial newLoading, UnloadingCargoMaterial newUnloading)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));

            if (notification == null)
                throw new BusinessRuleValidationException("Vessel Visit Notification not found.");

            // Se está a atualizar unloading cargo, validar capacidade
            if (newUnloading != null)
            {
                var vesselType = await _vesselTypeRepo.GetByIdAsync(notification.Vessel.VesselTypeId);
                if (vesselType == null)
                    throw new BusinessRuleValidationException("Vessel type not found.");

                var unloadingWeight = newUnloading.TotalWeightKg();
                var vesselCapacity = vesselType.Capacity;
                
                if (unloadingWeight > vesselCapacity)
                    throw new BusinessRuleValidationException(
                        $"Unloading cargo weight ({unloadingWeight} kg) cannot exceed vessel capacity ({vesselCapacity} kg).");
            }

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
