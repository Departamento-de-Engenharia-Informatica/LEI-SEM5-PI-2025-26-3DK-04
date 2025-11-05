using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.Vessels;
using DDDSample1.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.Vessels
{
    public class VesselRepository : BaseRepository<Vessel, VesselId>, IVesselRepository
    {
        private readonly DbSet<Vessel> _dbSet;

        public VesselRepository(DDDSample1DbContext context) : base(context.Vessels,context)
        {
            _dbSet = context.Vessels;
        }

        public async Task<Vessel> GetByImoNumberAsync(string imoNumber)
        {
            return await _dbSet
                .AsNoTracking()
                .Where(v => v.ImoNumber == imoNumber)
                .FirstOrDefaultAsync();
        }

        public async Task<List<Vessel>> SearchByNameAsync(string name)
        {
            return await _dbSet
                .Where(v => v.Name.ToLower().Contains(name.ToLower()))
                .ToListAsync();
        }

        public async Task<List<Vessel>> SearchByOwnerAsync(string owner)
        {
            return await _dbSet
                .Where(v => v.Owner.ToLower().Contains(owner.ToLower()))
                .ToListAsync();
        }

        public async Task<List<Vessel>> SearchByOperatorAsync(string operatorName)
        {
            return await _dbSet
                .Where(v => v.Operator.ToLower().Contains(operatorName.ToLower()))
                .ToListAsync();
        }

        public async Task<List<Vessel>> SearchAsync(string searchTerm)
        {
            var lowerSearchTerm = searchTerm.ToLower();
            
            // Search by IMO number, name, owner, or operator
            return await _dbSet
                .Where(v => v.ImoNumber.ToLower().Contains(lowerSearchTerm) ||
                           v.Name.ToLower().Contains(lowerSearchTerm) ||
                           v.Owner.ToLower().Contains(lowerSearchTerm) ||
                           v.Operator.ToLower().Contains(lowerSearchTerm))
                .ToListAsync();
        }

        public async Task<bool> ExistsByImoNumberAsync(string imoNumber)
        {
            return await _dbSet
                .AnyAsync(v => v.ImoNumber == imoNumber);
        }
    }
}
