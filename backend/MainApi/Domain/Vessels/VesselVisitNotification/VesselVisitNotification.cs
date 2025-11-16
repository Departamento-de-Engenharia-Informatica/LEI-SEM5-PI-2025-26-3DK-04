using System;
using System.Collections.Generic;
using System.Linq;
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

        // Novos atributos para integração com IARTI
        public DateTime? ArrivalTime { get; private set; }
        public DateTime? DepartureTime { get; private set; }
        public int? UnloadTime { get; private set; } // Em horas
        public int? LoadTime { get; private set; } // Em horas

        // Associações necessárias
        private List<string> _staffMemberIds = new List<string>();
        public IReadOnlyCollection<string> StaffMemberIds => _staffMemberIds.AsReadOnly();

        public string PhysicalResourceId { get; private set; } // Crane ID
        public string DockId { get; private set; }

        //private List<CargoManifest> _cargoManifests;

        private VesselVisitNotification()
        {
        }

        public VesselVisitNotification(
            Vessel vessel,
            LoadingCargoMaterial loadingCargo,
            UnloadingCargoMaterial unloadingCargo,
            RepresentativeId representativeId,
            DateTime arrivalTime,
            DateTime departureTime,
            List<string> staffMemberIds = null,
            string physicalResourceId = null,
            int? physicalResourceSetupTime = null,
            string dockId = null)
        {
            if (vessel == null)
                throw new BusinessRuleValidationException("Vessel is required for a visit notification.");

            if (representativeId == null)
                throw new BusinessRuleValidationException("Representative is required for a visit notification.");

            // Validar que arrival time não é no passado
            if (arrivalTime < DateTime.UtcNow)
                throw new BusinessRuleValidationException("Arrival time cannot be in the past.");

            // Validar que departure time não é no passado
            if (departureTime < DateTime.UtcNow)
                throw new BusinessRuleValidationException("Departure time cannot be in the past.");

            // Validar que departure é posterior ao arrival
            if (departureTime <= arrivalTime)
                throw new BusinessRuleValidationException("Departure time must be after arrival time.");

            this.Id = new VesselVisitNotificationID(Guid.NewGuid());
            this.Vessel = vessel;
            this.LoadingCargo = loadingCargo ?? new LoadingCargoMaterial(new List<CargoManifest>());
            this.UnloadingCargo = unloadingCargo ?? new UnloadingCargoMaterial(new List<CargoManifest>());
            this.RepresentativeId = representativeId;
            this.Status = NotificationStatus.InProgress;
            this.CreatedAt = DateTime.UtcNow;

            // Definir tempos de chegada e partida
            this.ArrivalTime = arrivalTime;
            this.DepartureTime = departureTime;

            // Associações opcionais
            if (staffMemberIds != null && staffMemberIds.Count > 0)
            {
                this._staffMemberIds = new List<string>(staffMemberIds);
            }

            this.PhysicalResourceId = physicalResourceId;
            this.DockId = dockId;

            // Calcular tempos de carga e descarga
            CalculateLoadAndUnloadTimes(physicalResourceSetupTime);
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

        public void UpdateInProgress(Vessel vessel, LoadingCargoMaterial loadingCargo,
            UnloadingCargoMaterial unloadingCargo)
        {
            if (this.Status != NotificationStatus.InProgress)
                throw new BusinessRuleValidationException(
                    "Only notifications in progress can be updated by a representative.");

            if (vessel == null && loadingCargo == null && unloadingCargo == null)
                throw new BusinessRuleValidationException(
                    "At least one field (Vessel, LoadingCargo, or UnloadingCargo) must be provided for update.");
            UpdateLoadingCargo(loadingCargo);
            UpdateVessel(vessel);
            UpdateUnloadingCargo(unloadingCargo);
        }

        public void SubmitForApproval()
        {
            if (this.Status != NotificationStatus.InProgress)
                throw new BusinessRuleValidationException(
                    "Only notifications in progress can be submitted for approval.");

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

        /// <summary>
        /// Calcula os tempos de carga e descarga baseado na quantidade de carga e no setup time do physical resource (crane).
        /// Fórmula com setup time: Tempo = (Quantidade de containers * SetupTime) em horas
        /// Fórmula sem setup time: Tempo = Quantidade de containers em horas
        /// </summary>
        private void CalculateLoadAndUnloadTimes(int? physicalResourceSetupTime)
        {
            // Se não houver setup time, usar 1 hora por container
            int setupTime = (physicalResourceSetupTime.HasValue && physicalResourceSetupTime.Value > 0)
                ? physicalResourceSetupTime.Value
                : 1;

            // Calcular unload time baseado na quantidade de containers no UnloadingCargo
            if (this.UnloadingCargo != null && this.UnloadingCargo.Manifests.Any())
            {
                int totalUnloadContainers = this.UnloadingCargo.Manifests
                    .Sum(m => m.Containers.Count);
                this.UnloadTime = totalUnloadContainers * setupTime; // Em horas
            }
            else
            {
                this.UnloadTime = 0;
            }

            // Calcular load time baseado na quantidade de containers no LoadingCargo
            if (this.LoadingCargo != null && this.LoadingCargo.Manifests.Any())
            {
                int totalLoadContainers = this.LoadingCargo.Manifests
                    .Sum(m => m.Containers.Count);
                this.LoadTime = totalLoadContainers * setupTime; // Em horas
            }
            else
            {
                this.LoadTime = 0;
            }
        }
// Dentro da classe VesselVisitNotification

// ... (métodos existentes)

// --- Novos Métodos de Update para IARTI/Planejamento ---

        public void UpdateSchedule(DateTime? arrivalTime, DateTime? departureTime)
        {
            if (this.Status != NotificationStatus.InProgress)
                throw new BusinessRuleValidationException(
                    "Only notifications in progress can be updated by a representative.");

            if (arrivalTime.HasValue && arrivalTime.Value < DateTime.UtcNow)
                throw new BusinessRuleValidationException("New arrival time cannot be in the past.");

            if (departureTime.HasValue && departureTime.Value < DateTime.UtcNow)
                throw new BusinessRuleValidationException("New departure time cannot be in the past.");

            // Usar os valores atuais se os novos forem nulos
            DateTime currentArrival = arrivalTime ?? this.ArrivalTime ?? DateTime.MinValue;
            DateTime currentDeparture = departureTime ?? this.DepartureTime ?? DateTime.MinValue;

            if (currentDeparture <= currentArrival)
                throw new BusinessRuleValidationException("Departure time must be after arrival time.");

            if (arrivalTime.HasValue)
                this.ArrivalTime = arrivalTime;

            if (departureTime.HasValue)
                this.DepartureTime = departureTime;
        }

        public void UpdateResources(List<string> staffMemberIds, string physicalResourceId, string dockId,
            int? physicalResourceSetupTime)
        {
            if (this.Status != NotificationStatus.InProgress)
                throw new BusinessRuleValidationException(
                    "Only notifications in progress can be updated by a representative.");

            // Atualizar PhysicalResourceId e DockId se fornecidos
            if (physicalResourceId != null)
                this.PhysicalResourceId = physicalResourceId;

            if (dockId != null)
                this.DockId = dockId;

            // Atualizar StaffMemberIds
            if (staffMemberIds != null)
            {
                this._staffMemberIds = new List<string>(staffMemberIds);
            }

            // Recalcular tempos de carga/descarga, pois o setup time pode ter mudado
            CalculateLoadAndUnloadTimes(physicalResourceSetupTime);
        }

        /// <summary>
        /// Permite o recálculo dos tempos de carga/descarga após uma alteração no Vessel, Carga ou Physical Resource.
        /// </summary>
        public void RecalculateLoadAndUnloadTimes(int? physicalResourceSetupTime)
        {
            CalculateLoadAndUnloadTimes(physicalResourceSetupTime);
        }
        
    }
}