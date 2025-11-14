using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Organizations
{
    public interface IRepresentativeRepository : IRepository<Representative, RepresentativeId>
    {
        // Buscar todos representantes ativos
        Task<List<Representative>> GetActiveRepresentativesAsync();

        // Buscar representantes por OrganizationId
        Task<List<Representative>> GetByOrganizationAsync(OrganizationId organizationId);

        // Buscar representante por email (opcional, útil para validação)
        Task<Representative> GetByEmailAsync(string email);
        Task<bool> ExistsWithEmailAsync(string email);
        Task<bool> ExistsWithPhoneAsync(string phoneNumber);
        Task<bool> ExistsWithCidAsync(string cid);
        Task DeleteAsync(Representative rep);
        Task<Representative> GetRepresentativeByEmailAsync(string email);
        Task<Representative> GetRepresentativeByCitizenCardAsync(string citizenCard);
        Task<Representative> GetRepresentativeByPhoneAsync(string phone);
    }
}