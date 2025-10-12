using System;

namespace DDDSample1.Domain.StaffMembers
{
    /// <summary>
    /// DTO para remover uma qualificação de um StaffMember (Opcional)
    /// Usado em: DELETE /api/staffmembers/{id}/qualifications/{qualificationId}
    /// NOTA: Pode-se usar apenas o route parameter em vez deste DTO
    /// </summary>
    public class RemoveQualificationDto
    {
        public Guid QualificationId { get; set; }
    }
}
