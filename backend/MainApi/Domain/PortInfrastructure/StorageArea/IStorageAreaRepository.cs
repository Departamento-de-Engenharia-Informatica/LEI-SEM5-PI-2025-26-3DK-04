using DDDSample1.Domain.Shared;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.PortInfrastructure.StorageArea;
using DDDSample1.Domain.Docks;

namespace DDDSample1.Domain.PortInfrastructure.StorageArea
{
    public interface IStorageAreaRepository : IRepository<StorageArea, StorageAreaID>
    {
        Task<List<StorageArea>> GetAllAsync();
        Task<StorageArea> GetByIdAsync(StorageAreaID id);
        Task<StorageArea> AddAsync(StorageArea area);
        Task<StorageArea> UpdateAsync(StorageArea area);
        void Remove(StorageArea area);
        Task<StorageArea?> GetByCodeAsync(string code);
        Task<List<StorageArea>> GetByStorageTypeAsync(StorageAreaType storageType);
    }
}
