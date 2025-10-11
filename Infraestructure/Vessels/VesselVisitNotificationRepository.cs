using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.Vessels;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.Vessels
{
    public class VesselVisitNotificationRepository : BaseRepository<VesselVisitNotification, VesselVisitNotificationID>, IVesselVisitNotificationRepository
    {
        public VesselVisitNotificationRepository(DDDSample1DbContext context) : base(context.VesselVisitNotifications)
        {
        }
        
        public async Task<List<VesselVisitNotification>> GetCompletedNotificationsAsync()
        {
            return await _objs
                .Where(n => n.State == NotificationState.Completed)
                .ToListAsync();
        }
        
        public async Task<List<VesselVisitNotification>> GetByStateAsync(NotificationState state)
        {
            return await _objs
                .Where(n => n.State == state)
                .ToListAsync();
        }
    }
}
