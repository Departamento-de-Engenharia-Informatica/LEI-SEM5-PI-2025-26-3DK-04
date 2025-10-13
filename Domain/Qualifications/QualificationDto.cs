using System;

namespace DDDSample1.Domain.Qualifications
{
    /// <summary>
    /// DTO de resposta para Qualification
    /// </summary>
    public class QualificationDto
    {
        public Guid Id { get; set; }
        public string Name { get; set; }
        public string Description { get; set; }
    }
}
