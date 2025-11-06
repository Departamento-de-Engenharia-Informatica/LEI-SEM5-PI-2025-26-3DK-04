using System.ComponentModel.DataAnnotations;

namespace IartiApi.DTOs
{
    /// <summary>
    /// DTO for user registration
    /// </summary>
    public class UserRegistrationDto
    {
        [Required(ErrorMessage = "Name is required")]
        public string Name { get; set; }

        [Required(ErrorMessage = "Sex is required")]
        [RegularExpression("^(male|female)$", ErrorMessage = "Sex must be 'male' or 'female'")]
        public string Sex { get; set; }

        [Required(ErrorMessage = "Birth year is required")]
        [Range(1850, 10000, ErrorMessage = "Birth year must be between 1850 and 10000")]
        public int BirthYear { get; set; }
    }
}
