using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Vessels.VesselInformation;
using DDDSample1.Domain.Organizations;

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
            CreateNotificationDto dto)
        {
            // Procurar o vessel
            var vessel = await _vesselRepo.GetByIdAsync(new VesselId(dto.VesselId));
            if (vessel == null)
                throw new BusinessRuleValidationException("Vessel not found.");

            // Procurar o vessel type para obter a capacidade
            var vesselType = await _vesselTypeRepo.GetByIdAsync(vessel.VesselTypeId);
            if (vesselType == null)
                throw new BusinessRuleValidationException("Vessel type not found.");

            // If controller passed a crew list, update the vessel’s crew
            if (dto.Crew != null && dto.Crew.Any())
            {
                vessel.setCrew(dto.Crew); // You need to add this method in Vessel aggregate
                await _vesselRepo.UpdateAsync(vessel);
            }

            // Process Loading Manifests from DTO
            LoadingCargoMaterial? loadingCargo = null;
            if (dto.LoadingManifests != null && dto.LoadingManifests.Any())
            {
                var manifests = new List<CargoManifest>();
                // Loop through input manifest DTOs
                foreach (var manifestDto in dto.LoadingManifests)
                {
                    // Create Container domain objects from ContainerInputDtos
                    var containers = manifestDto.Containers.Select(containerDto =>
                        new Container( // Assuming ContainerInputDto has Guid Id
                            containerDto.PayloadWeight,
                            containerDto.ContentsDescription)
                    ).ToList();

                    // Create CargoManifest using the new constructor with the list of containers
                    var manifest = new CargoManifest(containers); // ID is generated inside
                    manifests.Add(manifest);
                }
                loadingCargo = new LoadingCargoMaterial(manifests);
            }

            // Process Unloading Manifests from DTO (similar logic)
            UnloadingCargoMaterial? unloadingCargo = null;
            if (dto.UnloadingManifests != null && dto.UnloadingManifests.Any())
            {
                var manifests = new List<CargoManifest>();
                foreach (var manifestDto in dto.UnloadingManifests)
                {
                    var containers = manifestDto.Containers.Select(containerDto =>
                        new Container(
                            containerDto.PayloadWeight,
                            containerDto.ContentsDescription)
                    ).ToList();

                    var manifest = new CargoManifest(containers);
                    manifests.Add(manifest);
                }
                unloadingCargo = new UnloadingCargoMaterial(manifests);
            }

            // Create the vessel visit notification
            // Validar que o unloading cargo não excede a capacidade do vessel
            if (unloadingCargo != null)
            {
                var unloadingWeight = unloadingCargo.TotalWeightKg();
                var vesselCapacity = vesselType.Capacity;
                
                if (unloadingWeight > vesselCapacity)
                    throw new BusinessRuleValidationException(
                        $"Unloading cargo weight ({unloadingWeight} kg) cannot exceed vessel capacity ({vesselCapacity} kg).");
            }

            var notification = new VesselVisitNotification(vessel, loadingCargo, unloadingCargo, new RepresentativeId(dto.RepresentativeId));

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
                AssignedDock = notification.AssignedDock,
                RejectedReason = notification.RejectedReason,
                DecisionTimeStamp = notification.DecisionTimeStamp,
                DecisionOutcome = notification.DecisionOutcome,
                OfficerId = notification.OfficerId,
                VesselId = notification.Vessel.Id.AsGuid(),
                VesselName = notification.Vessel.Name,
                VesselCallsign = notification.Vessel.ImoNumber.ToString(),
                RepresentativeId = notification.RepresentativeId?.AsString(),
                CreatedAt = notification.CreatedAt
            };
            return dto;
        }
        
         public async Task<VesselVisitNotificationDto> UpdateInProgressAsync(string Id,UpdateNotificationDto dto)
        {
            if (string.IsNullOrWhiteSpace(dto.VesselId) && dto.LoadingCargo == null && dto.UnloadingCargo == null)
            {
                throw new BusinessRuleValidationException("At least one field (VesselId, LoadingCargo, or UnloadingCargo) must be provided for update.");
            }
            var notificationId = new VesselVisitNotificationID(Guid.Parse(Id));
            var notification = await _repo.GetByIdAsync(notificationId);

            if (notification == null)
                throw new BusinessRuleValidationException("Vessel Visit Notification not found.");

            if (notification.Status != NotificationStatus.InProgress)
                throw new BusinessRuleValidationException("Only notifications with 'InProgress' status can be updated.");

            // --- Obter o vessel atual (ou o novo se for atualizado) ---
            Vessel vesselToUse = notification.Vessel;
            bool vesselChanged = false;

            if (!string.IsNullOrWhiteSpace(dto.VesselId))
            {
                var newVessel = await _vesselRepo.GetByIdAsync(new VesselId(Guid.Parse(dto.VesselId)));
                if (newVessel == null)
                    throw new BusinessRuleValidationException("Vessel not found.");
                vesselToUse = newVessel;
                vesselChanged = true;
            }

            // --- Reconstruct LoadingCargo ---
            LoadingCargoMaterial? loadingToUse = notification.LoadingCargo;
            if (dto.LoadingCargo != null)
            {
                var manifests = new List<CargoManifest>();
                foreach (var manifestDto in dto.LoadingCargo.Manifests)
                {
                    var containers = manifestDto.Containers.Select(containerDto =>
                        new Container( containerDto.PayloadWeight, containerDto.ContentsDescription)
                    ).ToList();

                    var manifest = new CargoManifest(containers);
                    manifests.Add(manifest);
                }
                loadingToUse = new LoadingCargoMaterial(manifests);
            }
            
            UnloadingCargoMaterial? unloadingToUse = notification.UnloadingCargo;
            if (dto.UnloadingCargo != null)
            {
                var manifests = new List<CargoManifest>();
                foreach (var manifestDto in dto.UnloadingCargo.Manifests)
                {
                    var containers = manifestDto.Containers.Select(containerDto =>
                       
                         new Container( containerDto.PayloadWeight, containerDto.ContentsDescription)
                    ).ToList();

                    var manifest = new CargoManifest(containers);
                    manifests.Add(manifest);
                }
                unloadingToUse = new UnloadingCargoMaterial(manifests);
            }
            
            var vesselType = await _vesselTypeRepo.GetByIdAsync(vesselToUse.VesselTypeId);
            if (vesselType == null)
                throw new BusinessRuleValidationException("Vessel type not found for the specified vessel.");

            double totalWeight = loadingToUse.TotalWeightKg() + unloadingToUse.TotalWeightKg();

            if (totalWeight > vesselType.Capacity)
                throw new BusinessRuleValidationException(
                    $"Total cargo weight ({totalWeight} kg) exceeds vessel capacity ({vesselType.Capacity} kg).");
            
            if (vesselChanged)
                notification.UpdateVessel(vesselToUse);

            if (dto.LoadingCargo != null)
                notification.UpdateLoadingCargo(loadingToUse);

            if (dto.UnloadingCargo != null)
                notification.UpdateUnloadingCargo(unloadingToUse);

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
        
        public async Task<VesselVisitNotificationDto> WithdrawRequestAsync(Guid id)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));
            if (notification == null)
                throw new BusinessRuleValidationException("Notification not found.");

            notification.Withdraw();
            await _unitOfWork.CommitAsync();

            return MapToDto(notification);
        }

        public async Task<VesselVisitNotificationDto> ResumeAsync(Guid id)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));
            if (notification == null)
                throw new BusinessRuleValidationException("Notification not found.");

            notification.Resume();
            await _unitOfWork.CommitAsync();

            return MapToDto(notification);
        }
        
        public async Task<VesselVisitNotificationDto> ResetToInProgressAsync(Guid id)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));

            if (notification == null)
                throw new BusinessRuleValidationException("Vessel Visit Notification not found.");

            notification.ResetToPending();

            await _unitOfWork.CommitAsync();
            return MapToDto(notification);
        }
        
        // US 2.2.10: Search and filter notifications
        public async Task<List<VesselVisitNotificationDto>> SearchNotificationsAsync(NotificationFilterDto filter)
        {
            VesselId vesselId = filter.VesselId.HasValue ? new VesselId(filter.VesselId.Value) : null;
            RepresentativeId representativeId = !string.IsNullOrWhiteSpace(filter.RepresentativeId) ? new RepresentativeId(filter.RepresentativeId) : null;
            OrganizationId organizationId = filter.OrganizationId.HasValue ? new OrganizationId(filter.OrganizationId.Value.ToString()) : null;
            
            var notifications = await _repo.SearchNotificationsAsync(
                vesselId,
                filter.Status,
                representativeId,
                organizationId,
                filter.StartDate,
                filter.EndDate);
            
            return notifications.Select(n => MapToDto(n)).ToList();
        }
    }
}
