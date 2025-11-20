using Microsoft.AspNetCore.Mvc;
using System.Threading.Tasks;
using DDDSample1.Domain.Authentication;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class PortLayoutController : ControllerBase
    {
        private readonly PortLayoutService _service;

        public PortLayoutController(PortLayoutService service)
        {
            _service = service;
        }

        // GET: api/PortLayout
        [HttpGet("get")]
        [AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<object>> GetPortLayout()
        {
            var layout = await _service.GeneratePortLayoutAsync();
            return Ok(layout);
        }
    }
}