using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Vessels
{
    public interface IVesselRepository : IRepository<Vessel, VesselId>
    {
        Task<Vessel> GetByImoNumberAsync(string imoNumber);
        Task<List<Vessel>> SearchByNameAsync(string name);
        Task<List<Vessel>> SearchByOwnerAsync(string owner);
        Task<List<Vessel>> SearchByOperatorAsync(string operatorName);
        Task<List<Vessel>> SearchAsync(string searchTerm);
        Task<bool> ExistsByImoNumberAsync(string imoNumber);
    }
}
