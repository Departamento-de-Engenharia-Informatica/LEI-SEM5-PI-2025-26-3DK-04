using System;
using DDDSample1.Domain.Vessels.VesselVisitNotification;

namespace DDDSample1.Domain.Vessels.VesselVisitNotification
{
    public class NotificationFilterDto
    {
        public Guid? VesselId { get; set; }
        public NotificationStatus? Status { get; set; }
        public string? RepresentativeId { get; set; }
        public Guid? OrganizationId { get; set; }
        public DateTime? StartDate { get; set; }
        public DateTime? EndDate { get; set; }
    }
}
