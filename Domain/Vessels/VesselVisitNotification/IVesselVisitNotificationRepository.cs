using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Organizations;

namespace DDDSample1.Domain.Vessels.VesselVisitNotification
{
    public interface IVesselVisitNotificationRepository : IRepository<VesselVisitNotification, VesselVisitNotificationID>
    {
        // Método específico para procurar notificações completadas (prontas para review)
        Task<List<VesselVisitNotification>> GetCompletedNotificationsAsync();
        
        // Método para procurar notificações por estado
        Task<List<VesselVisitNotification>> GetByStateAsync(NotificationStatus status);
        
        // Método para procurar notificações com filtros (US 2.2.10)
        Task<List<VesselVisitNotification>> SearchNotificationsAsync(
            VesselId vesselId = null,
            NotificationStatus? status = null,
            RepresentativeId representativeId = null,
            OrganizationId organizationId = null,
            DateTime? startDate = null,
            DateTime? endDate = null);
    }
}
