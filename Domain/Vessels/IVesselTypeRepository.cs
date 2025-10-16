using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels
{
    public interface IVesselTypeRepository : IRepository<VesselType, VesselTypeId>
    {
        Task<List<VesselType>> SearchByNameAsync(string name);
        Task<List<VesselType>> SearchByDescriptionAsync(string description);
        Task<List<VesselType>> SearchByNameOrDescriptionAsync(string searchTerm);
    }
}
