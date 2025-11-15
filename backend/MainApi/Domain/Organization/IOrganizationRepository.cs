using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Organizations
{
    public interface IOrganizationRepository : IRepository<Organization, OrganizationId>
    {
        // Método para procurar por número fiscal (único)
        Task<Organization> GetByTaxNumberAsync(string taxNumber);

        // Método para listar todas as organizações
        Task<List<Organization>> GetAllAsync();

        // Método para verificar se já existe organização com o mesmo nome legal
        Task<Organization> GetByLegalNameAsync(string legalName);
    }
}