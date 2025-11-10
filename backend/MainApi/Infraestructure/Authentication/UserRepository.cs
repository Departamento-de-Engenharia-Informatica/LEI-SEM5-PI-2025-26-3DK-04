using DDDSample1.Domain.Authentication;
using DDDSample1.Infrastructure.Shared;
using System.Threading.Tasks;
using System.Linq;
using Microsoft.EntityFrameworkCore;


namespace DDDSample1.Infrastructure.Authentication
{
    public class UserRepository : BaseRepository<User, UserID>, IUserRepository
    {
        private readonly DDDSample1DbContext _context;

        public UserRepository(DDDSample1DbContext context) : base(context.Users,context)
        {
            _context = context;

        }
        public async Task<User> GetByEmailAsync(string email)
        {
            var emailConverted = new UserID(email);

            return await _context.Users
                .Where(r => r.Id == emailConverted)
                .FirstOrDefaultAsync();
        }
    }
}