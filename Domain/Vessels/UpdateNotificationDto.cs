using DDDSample1.Domain.Vessels;

namespace DDDSample1.Domain.Vessels
{
    public class UpdateNotificationDto
    {
        public LoadingCargoMaterial LoadingCargo { get; set; }
        public UnloadingCargoMaterial UnloadingCargo { get; set; }
    }
}