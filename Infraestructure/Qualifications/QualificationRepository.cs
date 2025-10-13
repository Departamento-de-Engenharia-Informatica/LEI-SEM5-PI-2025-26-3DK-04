using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.Qualifications;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.Qualifications
{
    public class QualificationRepository : BaseRepository<Qualification, QualificationID>, IQualificationRepository
    {
        public QualificationRepository(DDDSample1DbContext context) : base(context.Qualifications)
        {
        }
        
        // Pesquisar por nome
        public async Task<List<Qualification>> GetByNameAsync(string name)
        {
            return await _objs
                .Where(q => q.Name.Contains(name))
                .ToListAsync();
        }
        
        // Verificar se uma qualificação já existe por nome
        public async Task<bool> ExistsByNameAsync(string name)
        {
            return await _objs
                .AnyAsync(q => q.Name.ToLower() == name.ToLower());
        }
    }
}
