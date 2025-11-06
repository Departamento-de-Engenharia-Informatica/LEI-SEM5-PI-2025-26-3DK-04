using System.ComponentModel.DataAnnotations;

namespace IartiApi.DTOs
{
    /// <summary>
    /// DTO for file upload
    /// </summary>
    public class FileUploadDto
    {
        [Required(ErrorMessage = "File content is required")]
        public string FileContent { get; set; }
    }
}
