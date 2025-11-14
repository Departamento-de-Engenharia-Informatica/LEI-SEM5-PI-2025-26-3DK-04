using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.Organizations;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.Organizations
{
    public class OrganizationRepository : BaseRepository<Organization, OrganizationId>, IOrganizationRepository
    {
        public OrganizationRepository(DDDSample1DbContext context)
            : base(context.Organizations,context)
        {
        }

        public async Task<bool> ExistsWithLegalNameAsync(string legalName)
        {
            return await _objs.AnyAsync(o => o.LegalName == legalName);
        }
        
        public async Task<bool> GetByTaxNumberAsync(string taxNumber)
        {
            return await _objs.AnyAsync(o => o.TaxNumber == taxNumber);
        }

        public async Task<List<Organization>> GetAllAsync()
        {
            return await _objs.Include(o => o.Representatives).ToListAsync();
        }

        public async Task<Organization> GetByIdAsync(OrganizationId id)
        {
            return await _objs
                .Include(o => o.Representatives)
                .FirstOrDefaultAsync(o => o.Id.Equals(id));
        }
    }
}