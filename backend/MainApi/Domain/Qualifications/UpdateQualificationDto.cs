using System.ComponentModel.DataAnnotations;

namespace DDDSample1.Domain.Qualifications
{
    /// <summary>
    /// DTO de atualização para Qualification
    /// Usado em: PUT /api/qualifications/{id}
    /// Name: free text with at least two words and a maximum length of 150
    /// </summary>
    public class UpdateQualificationDto
    {
        /// <summary>
        /// Name: free text with at least two words and a maximum length of 150 characters
        /// </summary>
        [StringLength(150, ErrorMessage = "Name must have a maximum length of 150 characters")]
        public string Name { get; set; }
    }
}