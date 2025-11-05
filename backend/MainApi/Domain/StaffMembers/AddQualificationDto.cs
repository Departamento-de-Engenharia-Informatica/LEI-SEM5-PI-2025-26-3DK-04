using System;

namespace DDDSample1.Domain.StaffMembers
{
    /// <summary>
    /// DTO para adicionar uma qualificação a um StaffMember
    /// Usado em: POST /api/staffmembers/{id}/qualifications
    /// </summary>
    public class AddQualificationDto
    {
        public Guid QualificationId { get; set; }
    }
}
