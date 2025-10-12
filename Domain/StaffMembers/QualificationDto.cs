using System;

namespace DDDSample1.Domain.StaffMembers
{
    /// <summary>
    /// DTO de resposta para Qualification
    /// Usado dentro de StaffMemberDto para representar qualificações
    /// </summary>
    public class QualificationDto
    {
        public Guid Id { get; set; }
        public string Name { get; set; }
        public string Description { get; set; }
    }
}
