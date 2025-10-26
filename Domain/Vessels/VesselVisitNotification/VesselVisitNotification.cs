using System;
using System.Collections.Generic;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Vessels.VesselInformation;
using DDDSample1.Domain.Organizations;

namespace DDDSample1.Domain.Vessels.VesselVisitNotification
{

    public class VesselVisitNotification : Entity<VesselVisitNotificationID>, IAggregateRoot
    {
        public Vessel Vessel { get; private set; }
        
        public LoadingCargoMaterial? LoadingCargo { get; private set; }
        
        public UnloadingCargoMaterial? UnloadingCargo { get; private set; }

        public string RejectedReason { get; private set; }

        public DateTime? DecisionTimeStamp { get; private set; } 

        public string DecisionOutcome { get; private set; }

        public NotificationStatus Status { get; private set; }

        public string AssignedDock { get; private set; }

        public string OfficerId { get; private set; }
        
        public RepresentativeId RepresentativeId { get; private set; }
        
        public DateTime CreatedAt { get; private set; }
        //private List<CargoManifest> _cargoManifests;

        private VesselVisitNotification()
        {

        }

        public VesselVisitNotification(Vessel vessel, LoadingCargoMaterial loadingCargo, UnloadingCargoMaterial unloadingCargo, RepresentativeId representativeId)
        {
            if (vessel == null)
                throw new BusinessRuleValidationException("Vessel is required for a visit notification.");
            
            if (representativeId == null)
                throw new BusinessRuleValidationException("Representative is required for a visit notification.");
                
            this.Id = new VesselVisitNotificationID(Guid.NewGuid());
            this.Vessel = vessel;
            this.LoadingCargo = loadingCargo ?? new LoadingCargoMaterial(new List<CargoManifest>());
            this.UnloadingCargo = unloadingCargo ?? new UnloadingCargoMaterial(new List<CargoManifest>());
            this.RepresentativeId = representativeId;
            this.Status = NotificationStatus.InProgress;
            this.CreatedAt = DateTime.UtcNow;
        }

        public void Approve(string dockId, string officerId)
        {
            // Validação: só pode aprovar se estiver Submitted
            if (this.Status != NotificationStatus.Submitted)
                throw new BusinessRuleValidationException(
                    "Only notifications marked as submitted can be approved.");

            // Validação: dock é obrigatório
            if (string.IsNullOrWhiteSpace(dockId))
                throw new BusinessRuleValidationException(
                    "A dock must be assigned when approving a notification.");

            // Validação: officer ID é obrigatório
            if (string.IsNullOrWhiteSpace(officerId))
                throw new BusinessRuleValidationException(
                    "Officer ID is required for approval.");

            // Atualizar estado
            this.Status = NotificationStatus.Approved;
            this.AssignedDock = dockId;
            this.OfficerId = officerId;
            this.DecisionTimeStamp = DateTime.UtcNow;
            this.DecisionOutcome = "Approved";
            this.RejectedReason = null; // Limpar se foi rejeitado antes
        }

        public void Reject(string reason, string officerId)
        {
            // Validação: só pode rejeitar se estiver Submitted
            if (this.Status != NotificationStatus.Submitted)
                throw new BusinessRuleValidationException(
                    "Only notifications marked as submitted can be rejected.");

            // Validação: razão é obrigatória
            if (string.IsNullOrWhiteSpace(reason))
                throw new BusinessRuleValidationException(
                    "A rejection reason must be provided.");

            // Validação: officer ID é obrigatório
            if (string.IsNullOrWhiteSpace(officerId))
                throw new BusinessRuleValidationException(
                    "Officer ID is required for rejection.");

            // Atualizar estado
            this.Status = NotificationStatus.Rejected;
            this.RejectedReason = reason;
            this.OfficerId = officerId;
            this.DecisionTimeStamp = DateTime.UtcNow;
            this.DecisionOutcome = "Rejected";
            this.AssignedDock = null; // Limpar dock se foi aprovado antes
        }
        
        public void ResetToPending()
        {
            if (this.Status != NotificationStatus.Rejected)
                throw new BusinessRuleValidationException(
                    "Only rejected notifications can be reset to pending.");
            
            this.Status = NotificationStatus.Pending;
            this.RejectedReason = null;
            this.OfficerId = null;
            this.DecisionTimeStamp = null;
            this.DecisionOutcome = null;
            this.AssignedDock = null;
        }
        public void UpdateInProgress(Vessel vessel,LoadingCargoMaterial loadingCargo, UnloadingCargoMaterial unloadingCargo)
        {
            if (this.Status != NotificationStatus.InProgress)
                throw new BusinessRuleValidationException("Only notifications in progress can be updated by a representative.");
            
            if (vessel == null && loadingCargo == null && unloadingCargo == null)
                throw new BusinessRuleValidationException("At least one field (Vessel, LoadingCargo, or UnloadingCargo) must be provided for update.");
            UpdateLoadingCargo(loadingCargo);
            UpdateVessel(vessel);
            UpdateUnloadingCargo(unloadingCargo);
        }

        public void SubmitForApproval()
        {
            if (this.Status != NotificationStatus.InProgress)
                throw new BusinessRuleValidationException("Only notifications in progress can be submitted for approval.");

            this.Status = NotificationStatus.Submitted;
        }
        
        public void Withdraw()
        {
            if (this.Status != NotificationStatus.InProgress)
                throw new BusinessRuleValidationException(
                    "Only notifications in progress for approval can be withdrawn.");
            this.Status = NotificationStatus.WithdrawnRequest;
        }
        
        public void Resume()
        {
            if (Status != NotificationStatus.WithdrawnRequest)
                throw new BusinessRuleValidationException("Only withdrawn notifications can be resumed.");

            Status = NotificationStatus.InProgress;
        }
        
        public void UpdateVessel(Vessel vessel)
        {   
            if (vessel == null)
                throw new BusinessRuleValidationException("Vessel cannot be null.");
            this.Vessel = vessel;
        }

        public void UpdateLoadingCargo(LoadingCargoMaterial cargo)
        {
            this.LoadingCargo = cargo ?? new LoadingCargoMaterial(new List<CargoManifest>());
        }

        public void UpdateUnloadingCargo(UnloadingCargoMaterial cargo)
        {
            this.UnloadingCargo = cargo ?? new UnloadingCargoMaterial(new List<CargoManifest>());
        }

        
    }
}