using System.ComponentModel.DataAnnotations;

namespace DDDSample1.Domain.Qualifications
{
    /// <summary>
    /// DTO de criação para Qualification
    /// Name: free text with at least two words and a maximum length of 150
    /// </summary>
    public class CreateQualificationDto
    {
        /// <summary>
        /// Name: free text with at least two words and a maximum length of 150 characters
        /// </summary>
        [Required(ErrorMessage = "Name is required")]
        [StringLength(150, ErrorMessage = "Name must have a maximum length of 150 characters")]
        public string Name { get; set; }
    }
}