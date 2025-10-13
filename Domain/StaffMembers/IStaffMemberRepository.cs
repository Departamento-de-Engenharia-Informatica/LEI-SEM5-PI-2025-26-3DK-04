using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.StaffMembers
{
    public interface IStaffMemberRepository : IRepository<StaffMember, StaffMemberID>
    {
        // Pesquisar por nome
        Task<List<StaffMember>> GetByNameAsync(string name);
        
        // Filtrar por status
        Task<List<StaffMember>> GetByStatusAsync(MemberStatus status);
        
        // Filtrar por qualificação específica
        Task<List<StaffMember>> GetByQualificationAsync(Guid qualificationId);
        
        // Pesquisa combinada (múltiplos filtros opcionais)
        Task<List<StaffMember>> SearchAsync(
            string name = null,
            MemberStatus? status = null,
            Guid? qualificationId = null);
        
        // Listar todos os staff members ativos
        Task<List<StaffMember>> GetActiveStaffAsync();
        
        // Listar todos incluindo inativos
        Task<List<StaffMember>> GetAllForAuditAsync();
    }
}
