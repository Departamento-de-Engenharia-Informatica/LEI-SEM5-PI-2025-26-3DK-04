using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Qualifications
{
    public interface IQualificationRepository : IRepository<Qualification, QualificationID>
    {
        // Pesquisar por nome
        Task<List<Qualification>> GetByNameAsync(string name);
        
        // Verificar se uma qualificação já existe por nome (evitar duplicados)
        Task<bool> ExistsByNameAsync(string name);
    }
}
