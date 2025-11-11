using System;
using System.Threading.Tasks;
using DDDSample1.Infrastructure.Shared;
using DDDSample1.Domain.Authentication;
using Microsoft.EntityFrameworkCore;

namespace DDDSample1.Infrastructure.Authentication
{
    public class UserActivationRepository : BaseRepository<UserActivation,UserActivationID>, IUserActivationRepository
    {
        private readonly DDDSample1DbContext _context;
        public UserActivationRepository(DDDSample1DbContext context) : base(context.UserActivations,context)
        {
            _context = context;

        }

        public async Task<UserActivation?> GetByTokenAsync(string token)
        {
            return await _context.Set<UserActivation>()
                .FirstOrDefaultAsync(a => a.Token == token);
        }
        public async Task DeleteAsync(UserActivation activation)
        {
            if (activation == null) return;

            _context.Set<UserActivation>().Remove(activation);
            await _context.SaveChangesAsync();
        }
    }
}