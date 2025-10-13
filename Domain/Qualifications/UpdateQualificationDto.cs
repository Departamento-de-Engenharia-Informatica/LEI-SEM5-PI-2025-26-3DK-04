namespace DDDSample1.Domain.Qualifications
{
    /// <summary>
    /// DTO de atualização para Qualification
    /// Usado em: PUT /api/qualifications/{id}
    /// </summary>
    public class UpdateQualificationDto
    {
        public string Name { get; set; }
        public string Description { get; set; }
    }
}