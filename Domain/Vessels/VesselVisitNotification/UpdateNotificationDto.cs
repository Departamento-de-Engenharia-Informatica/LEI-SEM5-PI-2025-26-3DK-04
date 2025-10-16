using DDDSample1.Domain.Vessels.VesselInformation;

namespace DDDSample1.Domain.Vessels.VesselVisitNotification
{
    public class UpdateNotificationDto
    {
        public LoadingCargoMaterial LoadingCargo { get; set; }
        public UnloadingCargoMaterial UnloadingCargo { get; set; }
    }
}