using System;
using System.Threading.Tasks;
using DDDNetCore.Domain.PortInfrastructure.StorageArea;
using DDDNetCore.Infraestructure.PortInfrastructure.DTOs;
using DDDSample1.Domain.Docks;
using Microsoft.AspNetCore.Mvc;

namespace DDDSample1.Controllers;

[Route("api/[controller]")]
[ApiController]
public class StorageAreaController : ControllerBase
{
    private readonly StorageAreaService _service;

    public StorageAreaController(StorageAreaService service)
    {
        _service = service;
    }

    [HttpPost]
    public async Task<ActionResult<StorageAreaDto>> Create(StorageAreaDto dto)
    {
        var result = await _service.RegisterAsync(dto);
        return CreatedAtAction(nameof(GetById), new { id = result.Id }, result);
    }

    [HttpPut("{id}")]
    public async Task<ActionResult<StorageAreaDto>> Update(Guid id, StorageAreaDto dto)
    {
        if (id != dto.Id) return BadRequest("ID mismatch.");
        var result = await _service.UpdateAsync(id, dto);
        return result == null ? NotFound() : Ok(result);
    }

    [HttpGet("{id}")]
    public async Task<ActionResult<StorageAreaDto>> GetById(Guid id)
    {
        var result = await _service.GetByIdAsync(new StorageAreaID(id));
        return result == null ? NotFound() : Ok(result);
    }
}
