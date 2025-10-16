using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Vessels.VesselInformation;

namespace DDDSample1.Domain.Vessels
{
    public class VesselVisitNotificationService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly IVesselVisitNotificationRepository _repo;

        public VesselVisitNotificationService(IUnitOfWork unitOfWork, IVesselVisitNotificationRepository repo)
        {
            _unitOfWork = unitOfWork;
            _repo = repo;
        }

        //Create new notification
        public async Task<VesselVisitNotificationDto> CreateAsync(
            List<CargoManifest>? loadingManifests,
            List<CargoManifest>? unloadingManifests,
            List<CrewMember> crewMembers)
        {
            // Validate container IDs
            ValidateContainerIds(loadingManifests);
            ValidateContainerIds(unloadingManifests);

            var loadingCargo = loadingManifests != null && loadingManifests.Any()
                ? new LoadingCargoMaterial(loadingManifests)
                : null;

            var unloadingCargo = unloadingManifests != null && unloadingManifests.Any()
                ? new UnloadingCargoMaterial(unloadingManifests)
                : null;

            var notification = new VesselVisitNotification(loadingCargo, unloadingCargo);

            // Optional crew
            if (crewMembers != null && crewMembers.Any())
            {
                foreach (var crew in crewMembers)
                    notification.AddCrewMember(crew);
            }

            notification.MarkAsInProgress();

            await _repo.AddAsync(notification);
            await _unitOfWork.CommitAsync();

            return MapToDto(notification);
        }

        //Validate container IDs
        private void ValidateContainerIds(List<CargoManifest>? manifests)
        {
            if (manifests == null) return;

            foreach (var manifest in manifests)
            {
                foreach (var container in manifest.Containers)
                {
                    var _ = new ContainerID(container.Id.AsString()); // throws if invalid
                }
            }
        }

        //Submit for approval
        public async Task<VesselVisitNotificationDto> SubmitForApprovalAsync(Guid id)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));
            if (notification == null)
                throw new BusinessRuleValidationException("Vessel Visit Notification not found.");

            notification.SubmitForApproval();
            await _unitOfWork.CommitAsync();
            return MapToDto(notification);
        }

        //Update cargo while in progress
        public async Task<VesselVisitNotificationDto> UpdateInProgressAsync(Guid id, LoadingCargoMaterial? newLoading, UnloadingCargoMaterial? newUnloading)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));
            if (notification == null)
                throw new BusinessRuleValidationException("Vessel Visit Notification not found.");

            notification.UpdateInProgress(newLoading, newUnloading);
            await _unitOfWork.CommitAsync();
            return MapToDto(notification);
        }

        //Approve
        public async Task<VesselVisitNotificationDto> ApproveAsync(Guid id, Dock dock, string officerId)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));
            if (notification == null)
                throw new BusinessRuleValidationException("Vessel Visit Notification not found.");

            notification.Approve(dock, officerId);
            await _unitOfWork.CommitAsync();
            return MapToDto(notification);
        }

        // Reject
        public async Task<VesselVisitNotificationDto> RejectAsync(Guid id, string reason, string officerId)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));
            if (notification == null)
                throw new BusinessRuleValidationException("Vessel Visit Notification not found.");

            notification.Reject(reason, officerId);
            await _unitOfWork.CommitAsync();
            return MapToDto(notification);
        }

        // Get by ID
        public async Task<VesselVisitNotificationDto> GetByIdAsync(Guid id)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));
            return notification == null ? null : MapToDto(notification);
        }

        // List completed
        public async Task<List<VesselVisitNotificationDto>> GetCompletedNotificationsAsync()
        {
            var notifications = await _repo.GetCompletedNotificationsAsync();
            return notifications.Select(MapToDto).ToList();
        }

        // DTO mapper
        private VesselVisitNotificationDto MapToDto(VesselVisitNotification notification)
        {
            return new VesselVisitNotificationDto()
            {
                Id = notification.Id.AsGuid(),
                Status = notification.Status,
                AssignedDock = notification.AssignedDock,
                RejectedReason = notification.RejectedReason,
                DecisionTimeStamp = notification.DecisionTimeStamp,
                DecisionOutcome = notification.DecisionOutcome,
                OfficerId = notification.OfficerId,
                LoadingCargo = notification.LoadingCargo,
                UnloadingCargo = notification.UnloadingCargo,
                CrewMembers = notification._crewMembers?.Select(c => new CrewMemberDto
                {
                    Name = c.Name,
                    CitizenId = c.CitizenId,
                    Nationality = c.Nationality
                }).ToList()
            };
        }
    }
}
