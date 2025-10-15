using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels
{
    public interface IVesselVisitNotificationRepository : IRepository<VesselVisitNotification, VesselVisitNotificationID>
    {
        // Método específico para procurar notificações completadas (prontas para review)
        Task<List<VesselVisitNotification>> GetCompletedNotificationsAsync();
        
        // Método para procurar notificações por estado
        Task<List<VesselVisitNotification>> GetByStateAsync(NotificationStatus status);
    }
}
