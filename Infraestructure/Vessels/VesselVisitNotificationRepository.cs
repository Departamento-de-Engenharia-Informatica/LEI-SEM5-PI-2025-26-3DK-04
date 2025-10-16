using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.Vessels.VesselVisitNotification;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.Vessels
{
    public class VesselVisitNotificationRepository : BaseRepository<VesselVisitNotification, VesselVisitNotificationID>, IVesselVisitNotificationRepository
    {
        public VesselVisitNotificationRepository(DDDSample1DbContext context) : base(context.VesselVisitNotifications,context)
        {
        }
        
        public async Task<List<VesselVisitNotification>> GetCompletedNotificationsAsync()
        {
            return await _objs
                .Where(n => n.Status == NotificationStatus.Completed)
                .ToListAsync();
        }
        
        public async Task<List<VesselVisitNotification>> GetByStateAsync(NotificationStatus status)
        {
            return await _objs
                .Where(n => n.Status == status)
                .ToListAsync();
        }
    }
}
