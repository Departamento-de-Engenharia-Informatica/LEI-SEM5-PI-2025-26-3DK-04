using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Docks;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.PortInfrastructure.StorageArea;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.StorageAreas
{
    public class StorageAreaRepository : BaseRepository<StorageArea, StorageAreaID>, IStorageAreaRepository
    {
        private readonly DDDSample1DbContext _context;

        public StorageAreaRepository(DDDSample1DbContext context): base(context.StorageAreas, context)
        {
            _context = context;
        }

        public async Task<List<StorageArea>> GetAllAsync()
        {
            return await _context.StorageAreas.ToListAsync();
        }

        public async Task<StorageArea> GetByIdAsync(StorageAreaID id)
        {
            return await _context.StorageAreas
                .FirstOrDefaultAsync(sa => sa.Id.Equals(id));
        }

        public async Task<StorageArea> AddAsync(StorageArea area)
        {
            var result = await _context.StorageAreas.AddAsync(area);
            await _context.SaveChangesAsync();
            return result.Entity;
        }

        public async Task<StorageArea> UpdateAsync(StorageArea area)
        {
            _context.StorageAreas.Update(area);
            await _context.SaveChangesAsync();
            return area;
        }

        public void Remove(StorageArea area)
        {
            _context.StorageAreas.Remove(area);
            _context.SaveChanges();
        }
    }
}
