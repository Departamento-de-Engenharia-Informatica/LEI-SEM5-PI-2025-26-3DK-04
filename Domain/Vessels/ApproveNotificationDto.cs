using DDDSample1.Domain.Docks;

namespace DDDSample1.Domain.Vessels
{
    public class ApproveNotificationDto
    {
        public Dock DockId { get; set; }
        public string OfficerId { get; set; }
    }
}
