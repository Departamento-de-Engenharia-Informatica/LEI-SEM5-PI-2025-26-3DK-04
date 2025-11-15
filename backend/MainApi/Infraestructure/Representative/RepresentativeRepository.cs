using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.Organizations;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.Organizations
{
    public class RepresentativeRepository : BaseRepository<Representative, RepresentativeId>, IRepresentativeRepository
    {
        public RepresentativeRepository(DDDSample1DbContext context)
            : base(context.Representatives, context)
        {
        }

        public async Task<List<Representative>> GetActiveRepresentativesAsync()
        {
            return await _objs
                .Where(r => r.Status == RepresentativeStatus.Active)
                .ToListAsync();
        }

        public async Task<List<Representative>> GetByOrganizationAsync(OrganizationId organizationId)
        {
            return await _objs
                .Where(r => r.OrganizationId == organizationId)
                .ToListAsync();
        }

        public async Task<Representative> GetByEmailAsync(string email)
        {
            return await _objs
                .FirstOrDefaultAsync(r => r.Email == email);
        }

        public async Task<Representative> GetByCitizenIdAsync(string citizenId)
        {
            return await _objs
                .FirstOrDefaultAsync(r =>r.Id.Equals(citizenId) );
        }

        public async Task<bool> ExistsWithEmailAsync(string email)
        {
            return await _objs.AnyAsync(r => r.Email == email);
        }

        public async Task<bool> ExistsWithPhoneAsync(string phoneNumber)
        {
            return await _objs.AnyAsync(r => r.PhoneNumber == phoneNumber);
        }

        public async Task<bool> ExistsWithCidAsync(string cid)
        {
            return await _objs.AnyAsync(r => r.Id.Equals(cid));
        }
        
        
        public async Task UpdateAsync(Representative rep)
        {
            _context.Entry(rep).State = EntityState.Modified;
            await _context.SaveChangesAsync();
        }

        public async Task DeleteAsync(Representative rep)
        {
            _objs.Remove(rep);
            await _context.SaveChangesAsync();
        }

        public async Task<Representative> GetRepresentativeByEmailAsync(string email)
        {
            return await GetByEmailAsync(email);
        }

        public async Task<Representative> GetRepresentativeByCitizenCardAsync(string citizenCard)
        {
            return await _objs
                .FirstOrDefaultAsync(r => r.Id.Value == citizenCard);
        }

        public async Task<Representative> GetRepresentativeByPhoneAsync(string phone)
        {
            return await _objs
                .FirstOrDefaultAsync(r => r.PhoneNumber == phone);
        }
    }
}
