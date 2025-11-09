using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Authentication
{
    public interface IUserRepository: IRepository<User, UserID>
    {
        Task<User> GetByEmailAsync(string email);

    }
}