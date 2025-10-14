using System;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels
{

    public class VesselVisitNotification : Entity<VesselVisitNotificationID>, IAggregateRoot
    {
        public LoadingCargoMaterial LoadingCargo { get; private set; }
        
        public UnloadingCargoMaterial UnloadingCargo { get; private set; }

        public string RejectedReason { get; private set; }

        public DateTime? DecisionTimeStamp { get; private set; } 

        public string DecisionOutcome { get; private set; }

        public NotificationState State { get; private set; }

        public Dock AssignedDock { get; private set; }

        public string OfficerId { get; private set; }

        private VesselVisitNotification()
        {

        }

        public VesselVisitNotification(LoadingCargoMaterial loadingCargo, UnloadingCargoMaterial unloadingCargo)
        {
            this.Id = new VesselVisitNotificationID(Guid.NewGuid());
            this.LoadingCargo = loadingCargo;
            this.UnloadingCargo = unloadingCargo;
            this.State = NotificationState.Pending;
        }

        public void Approve(string dockId, string officerId)
        {
            // Validação: só pode aprovar se estiver Completed
            if (this.State != NotificationState.Completed)
                throw new BusinessRuleValidationException(
                    "Only notifications marked as completed can be approved.");

            // Validação: dock é obrigatório
            if (string.IsNullOrWhiteSpace(dockId))
                throw new BusinessRuleValidationException(
                    "A dock must be assigned when approving a notification.");

            // Validação: officer ID é obrigatório
            if (string.IsNullOrWhiteSpace(officerId))
                throw new BusinessRuleValidationException(
                    "Officer ID is required for approval.");

            // Atualizar estado
            this.State = NotificationState.Approved;
            this.AssignedDock = dockId;
            this.OfficerId = officerId;
            this.DecisionTimeStamp = DateTime.UtcNow;
            this.DecisionOutcome = "Approved";
            this.RejectedReason = null; // Limpar se foi rejeitado antes
        }

        public void Reject(string reason, string officerId)
        {
            // Validação: só pode rejeitar se estiver Completed
            if (this.State != NotificationState.Completed)
                throw new BusinessRuleValidationException(
                    "Only notifications marked as completed can be rejected.");

            // Validação: razão é obrigatória
            if (string.IsNullOrWhiteSpace(reason))
                throw new BusinessRuleValidationException(
                    "A rejection reason must be provided.");

            // Validação: officer ID é obrigatório
            if (string.IsNullOrWhiteSpace(officerId))
                throw new BusinessRuleValidationException(
                    "Officer ID is required for rejection.");

            // Atualizar estado
            this.State = NotificationState.Rejected;
            this.RejectedReason = reason;
            this.OfficerId = officerId;
            this.DecisionTimeStamp = DateTime.UtcNow;
            this.DecisionOutcome = "Rejected";
            this.AssignedDock = null; // Limpar dock se foi aprovado antes
        }
        
        public void ResetToPending()
        {
            if (this.State != NotificationState.Rejected)
                throw new BusinessRuleValidationException(
                    "Only rejected notifications can be reset to pending.");
            
            this.State = NotificationState.Pending;
            this.RejectedReason = null;
            this.OfficerId = null;
            this.DecisionTimeStamp = null;
            this.DecisionOutcome = null;
            this.AssignedDock = null;
        }
        public void UpdateInProgress(LoadingCargoMaterial loadingCargo, UnloadingCargoMaterial unloadingCargo)
        {
            if (this.State != NotificationState.InProgress)
                throw new BusinessRuleValidationException("Only notifications in progress can be updated by a representative.");

            if (loadingCargo == null && unloadingCargo == null)
                throw new BusinessRuleValidationException("At least one cargo section must be provided to update.");
            
            if (loadingCargo != null)
                this.LoadingCargo = loadingCargo;

            if (unloadingCargo != null)
                this.UnloadingCargo = unloadingCargo;
        }

        public void SubmitForApproval()
        {
            if (this.State != NotificationState.InProgress)
                throw new BusinessRuleValidationException("Only notifications in progress can be submitted for approval.");

            this.State = NotificationState.Submitted;
        }


    }
    
}