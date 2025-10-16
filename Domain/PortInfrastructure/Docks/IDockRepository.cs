using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Vessels;

namespace DDDSample1.Domain.Docks
{
    public interface IDockRepository : IRepository<Dock, DockID>
    {
        Task<List<Dock>> SearchByNameAsync(string name);
        Task<List<Dock>> FilterByVesselTypeAsync(VesselTypeId typeId);
        Task<List<Dock>> FilterByLocationAsync(string locationQuery);
        Task<bool> ExistsByNameAsync(string name);
    }
}