using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Infrastructure.Shared;

namespace DDDSample1.Infrastructure.StaffMembers
{
    public class StaffMemberRepository : BaseRepository<StaffMember, StaffMemberID>, IStaffMemberRepository
    {
        public StaffMemberRepository(DDDSample1DbContext context) : base(context.StaffMembers, context)
        {
        }
        
        // Pesquisar por nome
        public async Task<List<StaffMember>> GetByNameAsync(string name)
        {
            return await _objs
                .Where(s => s.Name.ToLower().Contains(name.ToLower()))
                .ToListAsync();
        }
        
        // Filtrar por status
        public async Task<List<StaffMember>> GetByStatusAsync(MemberStatus status)
        {
            return await _objs
                .Where(s => s.Status == status)
                .ToListAsync();
        }
        
        // Filtrar por qualificação específica
        public async Task<List<StaffMember>> GetByQualificationAsync(Guid qualificationId)
        {
            return await _objs
                .Where(s => s.Qualifications.Any(q => q.Id.AsGuid() == qualificationId))
                .ToListAsync();
        }
        
        // Pesquisa combinada (múltiplos filtros opcionais)
        public async Task<List<StaffMember>> SearchAsync(
            string name = null,
            MemberStatus? status = null,
            Guid? qualificationId = null)
        {
            var query = _objs.AsQueryable();

            // Aplicar filtros apenas se foram fornecidos
            if (!string.IsNullOrWhiteSpace(name))
                query = query.Where(s => s.Name.ToLower().Contains(name.ToLower()));

            if (status.HasValue)
                query = query.Where(s => s.Status == status.Value);

            if (qualificationId.HasValue)
                query = query.Where(s => s.Qualifications.Any(q => q.Id.AsGuid() == qualificationId.Value));

            return await query.ToListAsync();
        }
        
        // Listar apenas staff members ativos
        public async Task<List<StaffMember>> GetActiveStaffAsync()
        {
            return await _objs
                .Where(s => s.Status == MemberStatus.Available)
                .ToListAsync();
        }
        
        // Listar todos (incluindo inativos) para auditoria
        public async Task<List<StaffMember>> GetAllForAuditAsync()
        {
            return await _objs.ToListAsync();
        }
    }
}
