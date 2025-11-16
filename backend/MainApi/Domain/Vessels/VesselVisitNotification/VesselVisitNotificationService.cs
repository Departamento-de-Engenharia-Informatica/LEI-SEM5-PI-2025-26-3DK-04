using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Vessels.VesselInformation;
using DDDSample1.Domain.Organizations;
using DDDSample1.Domain.PhysicalResources;
using DDDSample1.Domain.StaffMembers;

namespace DDDSample1.Domain.Vessels.VesselVisitNotification
{
    public class VesselVisitNotificationService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IVesselVisitNotificationRepository _repo;
        private readonly IVesselRepository _vesselRepo;
        private readonly IVesselTypeRepository _vesselTypeRepo;
        private readonly IRepresentativeRepository _representativeRepo;
        private readonly IPhysicalResourceRepository _physicalResourceRepo;
        private readonly IStaffMemberRepository _staffMemberRepo;

        public VesselVisitNotificationService(
            IUnitOfWork unitOfWork,
            IVesselVisitNotificationRepository repo,
            IVesselRepository vesselRepo,
            IVesselTypeRepository vesselTypeRepo,
            IRepresentativeRepository representativeRepo,
            IPhysicalResourceRepository physicalResourceRepo,
            IStaffMemberRepository staffMemberRepo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
            _vesselRepo = vesselRepo;
            _vesselTypeRepo = vesselTypeRepo;
            _representativeRepo = representativeRepo;
            _physicalResourceRepo = physicalResourceRepo;
            _staffMemberRepo = staffMemberRepo;
        }

        //Create new notification
        public async Task<VesselVisitNotificationDto> CreateAsync(CreateNotificationDto dto)
        {
            // Validar que arrival time não é no passado
            if (dto.ArrivalTime < DateTime.UtcNow)
                throw new BusinessRuleValidationException("Arrival time cannot be in the past.");

            // Validar que departure time não é no passado
            if (dto.DepartureTime < DateTime.UtcNow)
                throw new BusinessRuleValidationException("Departure time cannot be in the past.");

            // Validar que departure é posterior ao arrival
            if (dto.DepartureTime <= dto.ArrivalTime)
                throw new BusinessRuleValidationException("Departure time must be after arrival time.");

            //  Obter o vessel
            var vessel = await _vesselRepo.GetByIdAsync(new VesselId(dto.VesselId));
            if (vessel == null)
                throw new BusinessRuleValidationException("Vessel not found.");

            //  Obter o vessel type
            var vesselType = await _vesselTypeRepo.GetByIdAsync(vessel.VesselTypeId);
            if (vesselType == null)
                throw new BusinessRuleValidationException("Vessel type not found.");

            //  Verificar se o representante existe
            var representative = await _representativeRepo.GetByIdAsync(new RepresentativeId(dto.RepresentativeId));
            if (representative == null)
                throw new BusinessRuleValidationException("Representative not found.");

            //  Atualizar crew se vier no DTO
            if (dto.Crew != null && dto.Crew.Any())
            {
                var crewMembers = dto.Crew.Select(c => new CrewMember(c.Name, c.CitizenId, c.Nationality)).ToList();
                vessel.setCrew(crewMembers);
                await _vesselRepo.UpdateAsync(vessel);
            }

            // Criar LoadingCargoMaterial (opcional)
            LoadingCargoMaterial loadingCargo = null;
            if (dto.LoadingManifests != null && dto.LoadingManifests.Any())
            {
                var manifests = dto.LoadingManifests.Select(mDto =>
                    new CargoManifest(
                        mDto.Containers.Select(c => new Container(c.PayloadWeight, c.ContentsDescription)).ToList()
                    )
                ).ToList();

                loadingCargo = new LoadingCargoMaterial(manifests);
            }

            //  Criar UnloadingCargoMaterial (opcional)
            UnloadingCargoMaterial unloadingCargo = null;
            if (dto.UnloadingManifests != null && dto.UnloadingManifests.Any())
            {
                var manifests = dto.UnloadingManifests.Select(mDto =>
                    new CargoManifest(
                        mDto.Containers.Select(c => new Container(c.PayloadWeight, c.ContentsDescription)).ToList()
                    )
                ).ToList();

                unloadingCargo = new UnloadingCargoMaterial(manifests);
            }

            //  Validar capacidade do vessel (apenas unloading)
            if (unloadingCargo != null)
            {
                var totalUnloadWeight = unloadingCargo.TotalWeightKg();
                if (totalUnloadWeight > vesselType.Capacity)
                    throw new BusinessRuleValidationException(
                        $"Unloading cargo weight ({totalUnloadWeight} kg) cannot exceed vessel capacity ({vesselType.Capacity} kg).");
            }

            // Validar e obter Physical Resource (crane) se fornecido
            int? physicalResourceSetupTime = null;
            if (!string.IsNullOrWhiteSpace(dto.PhysicalResourceId))
            {
                var physicalResource =
                    await _physicalResourceRepo.GetByIdAsync(
                        new PhysicalResourceId(Guid.Parse(dto.PhysicalResourceId)));
                if (physicalResource == null)
                    throw new BusinessRuleValidationException("Physical Resource (crane) not found.");

                if (physicalResource.Type != "Crane")
                    throw new BusinessRuleValidationException("Physical Resource must be a Crane.");

                physicalResourceSetupTime = physicalResource.SetupTime;
            }

            // Validar staff members se fornecidos
            if (dto.StaffMemberIds != null && dto.StaffMemberIds.Any())
            {
                foreach (var staffId in dto.StaffMemberIds)
                {
                    var staffMember = await _staffMemberRepo.GetByIdAsync(new StaffMemberID(Guid.Parse(staffId)));
                    if (staffMember == null)
                        throw new BusinessRuleValidationException($"Staff member with ID {staffId} not found.");
                }
            }

            var notification = new VesselVisitNotification(
                vessel,
                loadingCargo,
                unloadingCargo,
                new RepresentativeId(dto.RepresentativeId),
                dto.ArrivalTime,
                dto.DepartureTime,
                dto.StaffMemberIds,
                dto.PhysicalResourceId,
                physicalResourceSetupTime,
                dto.DockId
            );

            await _repo.AddAsync(notification);
            await _unitOfWork.CommitAsync();

            return MapToDto(notification);
        }


        // Listar notificações submetidas (prontas para review)
        public async Task<List<VesselVisitNotificationDto>> GetSubmittedNotificationsAsync()
        {
            var notifications = await _repo.GetSubmittedNotificationsAsync();

            return notifications.Select(n => MapToDto(n)).ToList();
        }

        // Listar notificações InProgress
        public async Task<List<VesselVisitNotificationDto>> GetInProgressNotificationsAsync()
        {
            var notifications = await _repo.GetByStateAsync(NotificationStatus.InProgress);

            return notifications.Select(n => MapToDto(n)).ToList();
        }


        // Listar notificações aprovadas (prontas para scheduling)
        public async Task<List<VesselVisitNotificationDto>> GetApprovedNotificationsAsync()
        {
            var notifications = await _repo.GetByStateAsync(NotificationStatus.Approved);

            return notifications.Select(n => MapToDto(n)).ToList();
        }
        
        public async Task<List<VesselVisitNotificationDto>> GetWithdrawnAsync()
        {
            var notifications = await _repo.GetByStateAsync(NotificationStatus.WithdrawnRequest);

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
                CreatedAt = notification.CreatedAt,
                // Novos campos
                ArrivalTime = notification.ArrivalTime,
                DepartureTime = notification.DepartureTime,
                UnloadTime = notification.UnloadTime,
                LoadTime = notification.LoadTime,
                StaffMemberIds = notification.StaffMemberIds?.ToList(),
                PhysicalResourceId = notification.PhysicalResourceId,
                DockId = notification.DockId
            };
            return dto;
        }

        public async Task<VesselVisitNotificationDto> UpdateInProgressAsync(Guid Id, UpdateNotificationDto dto)
        {
            var notificationId = new VesselVisitNotificationID(Id);
            var notification = await _repo.GetByIdAsync(notificationId);

            if (notification == null)
                throw new BusinessRuleValidationException("Vessel Visit Notification not found.");

            if (notification.Status != NotificationStatus.InProgress)
                throw new BusinessRuleValidationException(
                    "Only notifications with 'InProgress' status can be updated.");

            // Flag para determinar se é necessário recalcular tempos de carga/descarga no final
            bool requiresTimeRecalculation = false;

            // ------------------------------------
            // 1. Atualizar Vessel e Crew (Se fornecidos)
            // ------------------------------------
            Vessel vesselToUse = notification.Vessel;

            if (!string.IsNullOrWhiteSpace(dto.VesselId) && dto.VesselId != vesselToUse.Id.AsString())
            {
                var newVessel = await _vesselRepo.GetByIdAsync(new VesselId(Guid.Parse(dto.VesselId)));
                if (newVessel == null)
                    throw new BusinessRuleValidationException("Vessel not found.");

                vesselToUse = newVessel;
                notification.UpdateVessel(vesselToUse);
                // O VesselType e capacidade serão verificados mais abaixo
            }

            if (dto.Crew != null)
            {
                if (dto.Crew.Any())
                {
                    var crewMembers = dto.Crew.Select(c => new CrewMember(c.Name, c.CitizenId, c.Nationality)).ToList();
                    vesselToUse.setCrew(crewMembers);
                }
                else
                {
                    // Se o DTO vier com uma lista vazia, pode-se assumir a intenção de limpar a crew
                    vesselToUse.setCrew(new List<CrewMember>());
                }

                await _vesselRepo.UpdateAsync(vesselToUse); // Persistir mudança na entidade Vessel
            }

            // ------------------------------------
            // 2. Atualizar Carga (Se fornecida)
            // ------------------------------------
            LoadingCargoMaterial? loadingToUse = notification.LoadingCargo;
            UnloadingCargoMaterial? unloadingToUse = notification.UnloadingCargo;

            if (dto.LoadingCargo != null)
            {
                requiresTimeRecalculation = true;
                var manifests = dto.LoadingCargo.Manifests.Select(manifestDto =>
                    new CargoManifest(manifestDto.Containers.Select(containerDto =>
                        new Container(containerDto.PayloadWeight, containerDto.ContentsDescription)
                    ).ToList())
                ).ToList();
                loadingToUse = new LoadingCargoMaterial(manifests);
                notification.UpdateLoadingCargo(loadingToUse);
            }

            if (dto.UnloadingCargo != null)
            {
                requiresTimeRecalculation = true;
                var manifests = dto.UnloadingCargo.Manifests.Select(manifestDto =>
                    new CargoManifest(manifestDto.Containers.Select(containerDto =>
                        new Container(containerDto.PayloadWeight, containerDto.ContentsDescription)
                    ).ToList())
                ).ToList();
                unloadingToUse = new UnloadingCargoMaterial(manifests);
                notification.UpdateUnloadingCargo(unloadingToUse);
            }

            // ------------------------------------
            // 3. Atualizar IARTI/Agendamento
            // ------------------------------------

            // Atualiza Arrival/Departure
            if (dto.ArrivalTime.HasValue || dto.DepartureTime.HasValue)
            {
                notification.UpdateSchedule(dto.ArrivalTime, dto.DepartureTime);
            }

            // Variáveis para atualização de recursos
            string physicalResourceId = dto.PhysicalResourceId ?? notification.PhysicalResourceId;
            int? physicalResourceSetupTime = null;

            // Validação e obtenção de SetupTime para Physical Resource (Crane)
            if (!string.IsNullOrWhiteSpace(physicalResourceId))
            {
                var physicalResource =
                    await _physicalResourceRepo.GetByIdAsync(new PhysicalResourceId(Guid.Parse(physicalResourceId)));
                if (physicalResource == null)
                    throw new BusinessRuleValidationException("Physical Resource (crane) not found.");

                if (physicalResource.Type != "Crane")
                    throw new BusinessRuleValidationException("Physical Resource must be a Crane.");

                physicalResourceSetupTime = physicalResource.SetupTime;
                requiresTimeRecalculation = true;
            }
            else if (dto.PhysicalResourceId == "") // Se for passado explicitamente vazio (limpar)
            {
                physicalResourceId = null;
                physicalResourceSetupTime = 1; // Usar setup time default para recalcular
                requiresTimeRecalculation = true;
            }


            // Validar staff members se fornecidos no DTO
            if (dto.StaffMemberIds != null)
            {
                foreach (var staffId in dto.StaffMemberIds)
                {
                    var staffMember = await _staffMemberRepo.GetByIdAsync(new StaffMemberID(Guid.Parse(staffId)));
                    if (staffMember == null)
                        throw new BusinessRuleValidationException($"Staff member with ID {staffId} not found.");
                }
            }

            // Atualiza Dock, Staff e Physical Resource
            if (dto.StaffMemberIds != null || dto.PhysicalResourceId != null || dto.DockId != null)
            {
                notification.UpdateResources(
                    dto.StaffMemberIds,
                    physicalResourceId,
                    dto.DockId ?? notification.DockId,
                    physicalResourceSetupTime
                );
            }

            // ------------------------------------
            // 4. Validação Final de Capacidade e Recálculo
            // ------------------------------------

            // Validação de Capacidade
            var vesselType = await _vesselTypeRepo.GetByIdAsync(vesselToUse.VesselTypeId);
            if (vesselType == null)
                throw new BusinessRuleValidationException("Vessel type not found for the specified vessel.");

            // Usar os valores de carga mais recentes (se foram atualizados, já estão no `notification`)
            double totalWeight =
                notification.LoadingCargo.TotalWeightKg() + notification.UnloadingCargo.TotalWeightKg();

            if (totalWeight > vesselType.Capacity)
                throw new BusinessRuleValidationException(
                    $"Total cargo weight ({totalWeight} kg) exceeds vessel capacity ({vesselType.Capacity} kg).");

            // Se a carga mudou, ou o physical resource mudou, ou se for uma limpeza de physical resource
            if (requiresTimeRecalculation)
            {
                notification.RecalculateLoadAndUnloadTimes(physicalResourceSetupTime);
            }

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
            RepresentativeId representativeId = !string.IsNullOrWhiteSpace(filter.RepresentativeId)
                ? new RepresentativeId(filter.RepresentativeId)
                : null;
            OrganizationId organizationId = filter.OrganizationId.HasValue
                ? new OrganizationId(filter.OrganizationId.Value.ToString())
                : null;

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