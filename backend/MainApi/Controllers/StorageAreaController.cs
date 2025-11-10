using System;
using System.Collections.Generic; // Added for List<>
using System.Threading.Tasks;
using DDDNetCore.Domain.PortInfrastructure.StorageArea.DTOs;
// Correct DTO namespaces
using DDDNetCore.Infraestructure.PortInfrastructure.DTOs;
using DDDSample1.Domain.Authentication; // For StorageAreaDto, DockAssignmentDto
using DDDSample1.Domain.PortInfrastructure.StorageArea; // For Create/Update/Assign DTOs
using DDDSample1.Domain.Docks; // For DockID
using DDDSample1.Domain.Shared; // For BusinessRuleValidationException
// Correct Service/Entity namespaces
using DDDSample1.Domain.PortInfrastructure.StorageArea;
using DDDSample1.Domain.StorageAreas.DTOs;
using Microsoft.AspNetCore.Mvc;

namespace DDDSample1.Controllers // Your controller namespace
{
    [Route("api/[controller]")]
    [ApiController]
    public class StorageAreaController : ControllerBase
    {
        private readonly StorageAreaService _service;

        public StorageAreaController(StorageAreaService service)
        {
            _service = service;
        }

        // GET: api/StorageArea
        [HttpGet]
        //[AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<IEnumerable<StorageAreaDto>>> GetAll()
        {
            // Assuming GetAllAsync returns only active ones or you filter in service
            return await _service.GetAllAsync();
        }

        // GET: api/StorageArea/{id}
        [HttpGet("{id:guid}")] // Specify GUID constraint
        //[AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<StorageAreaDto>> GetById(Guid id)
        {
            var result = await _service.GetByIdAsync(new StorageAreaID(id));
            if (result == null)
            {
                return NotFound(new { Message = $"Storage Area with ID {id} not found or inactive." });
            }
            return Ok(result);
        }

        // POST: api/StorageArea
        [HttpPost]
        //[AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<StorageAreaDto>> Create(CreateStorageAreaDto dto) // Use Create DTO
        {
            try
            {
                var result = await _service.AddAsync(dto);
                // Use the Id from the returned DTO for the location header
                return CreatedAtAction(nameof(GetById), new { id = result.Id }, result);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PUT: api/StorageArea/{id}
        [HttpPut("{id:guid}")] // Specify GUID constraint
        //[AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<StorageAreaDto>> Update(Guid id, UpdateStorageAreaDto dto) // Use Update DTO
        {
            // ID comes from the route, no need to check dto.Id if UpdateStorageAreaDto doesn't have it
            try
            {
                var result = await _service.UpdateAsync(new StorageAreaID(id), dto);
                if (result == null)
                {
                     return NotFound(new { Message = $"Storage Area with ID {id} not found or inactive." });
                }
                return Ok(result);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // --- New Endpoints ---

        // POST: api/StorageArea/{id}/assignDock
        [HttpPost("{id:guid}/assignDock")]
        //[AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<StorageAreaDto>> AssignDock(Guid id, AssignDockDto dto)
        {
             try
            {
                var result = await _service.AssignDockAsync(new StorageAreaID(id), dto);
                 if (result == null) // Should not happen if service throws exceptions correctly
                {
                     return NotFound(new { Message = $"Storage Area with ID {id} not found or inactive." });
                }
                return Ok(result);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // DELETE: api/StorageArea/{id}/unassignDock/{dockId}
        [HttpDelete("{id:guid}/unassignDock/{dockId:guid}")]
        //[AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<StorageAreaDto>> UnassignDock(Guid id, Guid dockId)
        {
             try
            {
                var result = await _service.UnassignDockAsync(new StorageAreaID(id), new DockID(dockId));
                 if (result == null) // Should not happen
                {
                     return NotFound(new { Message = $"Storage Area with ID {id} not found or inactive." });
                }
                return Ok(result); // Return updated area without the assignment
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }


        // PATCH: api/StorageArea/{id}/inactivate
        [HttpPatch("{id:guid}/inactivate")]
        //[AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<StorageAreaDto>> Inactivate(Guid id)
        {
             try
            {
                var result = await _service.InactivateAsync(new StorageAreaID(id));
                if (result == null)
                {
                    return NotFound(new { Message = $"Storage Area with ID {id} not found." });
                }
                return Ok(result); // Return the inactive area DTO
            }
            catch (BusinessRuleValidationException ex) // Catch if already inactive
            {
                 return BadRequest(new { Message = ex.Message });
            }
        }

         // PATCH: api/StorageArea/{id}/activate
        [HttpPatch("{id:guid}/activate")]
        //[AuthorizeRole(Roles.Admin)]
        public async Task<ActionResult<StorageAreaDto>> Activate(Guid id)
        {
             try
            {
                var result = await _service.ActivateAsync(new StorageAreaID(id));
                if (result == null)
                {
                    return NotFound(new { Message = $"Storage Area with ID {id} not found." });
                }
                return Ok(result); // Return the active area DTO
            }
            catch (BusinessRuleValidationException ex) // Catch if already active
            {
                 return BadRequest(new { Message = ex.Message });
            }
        }

        // Optional: Hard Delete (use cautiously)
        // [HttpDelete("{id:guid}")]
        // public async Task<ActionResult<StorageAreaDto>> Delete(Guid id)
        // {
        //     try
        //     {
        //         var result = await _service.DeleteAsync(new StorageAreaID(id)); // Assuming DeleteAsync exists
        //         if (result == null)
        //         {
        //             return NotFound();
        //         }
        //         return Ok(result); // Return the DTO of the deleted item
        //     }
        //     catch (BusinessRuleValidationException ex)
        //     {
        //          return BadRequest(new { Message = ex.Message });
        //     }
        // }
    }
}