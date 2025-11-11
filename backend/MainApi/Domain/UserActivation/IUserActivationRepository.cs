using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Authentication
{
    public interface IUserActivationRepository : IRepository<UserActivation,UserActivationID>
    {
        Task<UserActivation?> GetByTokenAsync(string token);
        Task DeleteAsync(UserActivation activation);
    }
}