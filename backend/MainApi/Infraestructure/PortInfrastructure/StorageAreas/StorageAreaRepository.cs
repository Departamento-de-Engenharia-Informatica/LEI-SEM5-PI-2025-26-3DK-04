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
        public async Task<StorageArea?> GetByCodeAsync(string code)
        {
            // Use _objs from BaseRepository
            return await _objs
                .Include(sa => sa.DockAssignments) // Include assignments if needed when fetching by code
                .FirstOrDefaultAsync(sa => sa.Code == code);
        }

        public async Task<List<StorageArea>> GetAllAsync()
        {
            return await _objs
                .Include(sa => sa.DockAssignments) // Eager load assignments
                // Optionally add .Where(sa => sa.Active)  if want only active ones
                .ToListAsync();
        }

        public async Task<StorageArea> GetByIdAsync(StorageAreaID id)
        {
            return await _objs
                .Include(sa => sa.DockAssignments) // Eager load assignments
                .FirstOrDefaultAsync(sa => sa.Id.Equals(id));
        }

        public async Task<StorageArea> AddAsync(StorageArea area)
        {
            var result = await _objs.AddAsync(area);
            //await _context.SaveChangesAsync();
            return result.Entity;
        }

        public async Task<StorageArea> UpdateAsync(StorageArea area)
        {
            _objs.Update(area);
            //await _context.SaveChangesAsync();
            return area;
        }

        public void Remove(StorageArea area)
        {
            _objs.Remove(area);
            // NO SaveChanges here - UnitOfWork handles it
        }
    }
}
