using DDDSample1.Domain.Vessels.VesselInformation;

namespace DDDSample1.Domain.Vessels.VesselVisitNotification
{
    public class UpdateNotificationDto
    {
        public string? VesselId { get; set; } 
        public LoadingCargoMaterialDto? LoadingCargo { get; set; }
        public UnloadingCargoMaterialDTO? UnloadingCargo { get; set; }
    }

}