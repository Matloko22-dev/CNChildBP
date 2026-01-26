using Microsoft.AspNetCore.Mvc;

namespace OraculoCorporativo.Controllers
{
    [ApiController]
    [Route("api/[controller]")]
    public class OraculoController : ControllerBase
    {
        [HttpGet]
        public IActionResult Get()
        {
            return Ok("API funcionando!");
        }

        [HttpPost("query")]
        public IActionResult QueryAI([FromBody] string question)
        {
            return Ok(new { Message = $"Você perguntou: {question}" });
        }
    }
}

