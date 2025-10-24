using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.Vessels.VesselVisitNotification;
using DDDSample1.Domain.Organizations;
using DDDSample1.Domain.Vessels;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.Vessels
{
    public class VesselVisitNotificationRepository : BaseRepository<VesselVisitNotification, VesselVisitNotificationID>, IVesselVisitNotificationRepository
    {
        public VesselVisitNotificationRepository(DDDSample1DbContext context) : base(context.VesselVisitNotifications,context)
        {
        }

        // Ensure GetByIdAsync includes navigation properties (Vessel) so service mapping won't get nulls
        public new async Task<VesselVisitNotification> GetByIdAsync(VesselVisitNotificationID id)
        {
            return await _objs
                .Include(n => n.Vessel)
                .Where(x => id.Equals(x.Id))
                .FirstOrDefaultAsync();
        }
        
        public async Task<List<VesselVisitNotification>> GetCompletedNotificationsAsync()
        {
            // Procurar notificações submetidas (prontas para aprovação/rejeição)
            return await _objs
                .Include(n => n.Vessel)
                .Where(n => n.Status == NotificationStatus.Submitted)
                .ToListAsync();
        }
        
        public async Task<List<VesselVisitNotification>> GetByStateAsync(NotificationStatus status)
        {
            return await _objs
                .Include(n => n.Vessel)
                .Where(n => n.Status == status)
                .ToListAsync();
        }
        
        public async Task<List<VesselVisitNotification>> SearchNotificationsAsync(
            VesselId vesselId = null,
            NotificationStatus? status = null,
            RepresentativeId representativeId = null,
            OrganizationId organizationId = null,
            DateTime? startDate = null,
            DateTime? endDate = null)
        {
            var query = _objs
                .Include(n => n.Vessel)
                .AsQueryable();

            // Filter by vessel
            if (vesselId != null)
            {
                query = query.Where(n => n.Vessel.Id == vesselId);
            }

            // Filter by status
            if (status.HasValue)
            {
                query = query.Where(n => n.Status == status.Value);
            }

            // Filter by representative
            if (representativeId != null)
            {
                query = query.Where(n => n.RepresentativeId == representativeId);
            }

            // Filter by organization (notifications from any representative in the organization)
            if (organizationId != null)
            {
                // Get all representatives from the organization
                var representativeIds = await ((DDDSample1DbContext)_context).Representatives
                    .Where(r => r.OrganizationId == organizationId)
                    .Select(r => r.Id)
                    .ToListAsync();

                query = query.Where(n => representativeIds.Contains(n.RepresentativeId));
            }

            // Filter by date range
            if (startDate.HasValue)
            {
                query = query.Where(n => n.CreatedAt >= startDate.Value);
            }

            if (endDate.HasValue)
            {
                query = query.Where(n => n.CreatedAt <= endDate.Value);
            }

            return await query
                .OrderByDescending(n => n.CreatedAt)
                .ToListAsync();
        }
    }
}
