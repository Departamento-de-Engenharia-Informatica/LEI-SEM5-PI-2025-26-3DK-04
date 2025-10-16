using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Vessels;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.Vessels
{
    public class VesselTypeRepository : BaseRepository<VesselType, VesselTypeId>, IVesselTypeRepository
    {
        private readonly DbSet<VesselType> _dbSet;

        public VesselTypeRepository(DDDSample1DbContext context) : base(context.VesselTypes)
        {
            _dbSet = context.VesselTypes;
        }

        public async Task<List<VesselType>> SearchByNameAsync(string name)
        {
            return await _dbSet
                .Where(vt => vt.Name.ToLower().Contains(name.ToLower()))
                .ToListAsync();
        }

        public async Task<List<VesselType>> SearchByDescriptionAsync(string description)
        {
            return await _dbSet
                .Where(vt => vt.Description.ToLower().Contains(description.ToLower()))
                .ToListAsync();
        }

        public async Task<List<VesselType>> SearchByNameOrDescriptionAsync(string searchTerm)
        {
            var lowerSearchTerm = searchTerm.ToLower();
            return await _dbSet
                .Where(vt => vt.Name.ToLower().Contains(lowerSearchTerm) || 
                            vt.Description.ToLower().Contains(lowerSearchTerm))
                .ToListAsync();
        }
    }
}
