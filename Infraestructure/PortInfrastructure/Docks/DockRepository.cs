using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.Docks;
using DDDSample1.Domain.Vessels;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.Docks
{
    public class DockRepository : BaseRepository<Dock, DockID>, IDockRepository
    {
        public DockRepository(DDDSample1DbContext context) : base(context.Docks, context)
        {
        }

        public async Task<List<Dock>> SearchByNameAsync(string name)
        {
            return await _objs
                .Where(d => d.Name.Contains(name))
                .ToListAsync();
        }

       

        public async Task<List<Dock>> FilterByVesselTypeAsync(VesselTypeId typeId)
        {
            return await _objs
                .Where(d => d.AllowedVesselTypes.Any(v => v.Id.Equals(typeId)))
                .ToListAsync();
        }

        public async Task<List<Dock>> FilterByLocationAsync(string locationQuery)
        {
            return await _objs
                .Where(d => d.Location.Description.Contains(locationQuery) || d.Location.Coordinates.Contains(locationQuery))
                .ToListAsync();
        }

        public async Task<bool> ExistsByNameAsync(string name)
        {
            return await _objs.AnyAsync(d => d.Name == name);
        }
        public async Task<List<Dock>> GetAllAsync()
        {
            return await _objs.Where(d => d.Active).ToListAsync();
        }

    }
}