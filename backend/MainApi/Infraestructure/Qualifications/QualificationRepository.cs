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
        public QualificationRepository(DDDSample1DbContext context) : base(context.Qualifications, context)
        {
        }
        
        // Pesquisar por nome
        public async Task<List<Qualification>> GetByNameAsync(string name)
        {
            return await _objs
                .Where(q => q.Name.ToLower().Contains(name.ToLower()))
                .ToListAsync();
        }
    }
}
