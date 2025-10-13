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
            : base(context.Organizations)
        {
        }

        // 🔍 Verifica se já existe organização com o mesmo nome legal
        public async Task<bool> ExistsWithLegalNameAsync(string legalName)
        {
            return await _objs.AnyAsync(o => o.LegalName == legalName);
        }

        // 🔍 Busca todas as organizações com seus representantes
        public Task<Organization> GetByTaxNumberAsync(string taxNumber)
        {
            throw new System.NotImplementedException();
        }

        public async Task<List<Organization>> GetAllAsync()
        {
            return await _objs.Include(o => o.Representatives).ToListAsync();
        }

        // 🔍 Busca organização por ID (incluindo representantes)
        public async Task<Organization> GetByIdAsync(OrganizationId id)
        {
            return await _objs
                .Include(o => o.Representatives)
                .FirstOrDefaultAsync(o => o.Id.Equals(id));
        }
    }
}