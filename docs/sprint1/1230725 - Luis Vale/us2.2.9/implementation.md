# 5. User Story Implementation Report

## 5.1. US_2.2.9 – Change or Complete a Vessel Visit Notification

### 5.2. Description

As a Shipping Agent Representative, I want to edit or complete a Vessel Visit Notification while it’s still in progress,
so I can correct mistakes or withdraw requests when necessary.

### 5.3. Implementation Details

- **User Interaction & Flow Control**
  Yet to implement, since it is not required this sprint, using swagger as a substitute to test the API.
- **Business Logic:**  
  `VesselVisitNotificationService` enforces state rules and handles validations.
- **Persistence:**  
  `IVesselVisitNotificationRepository` updates and retrieves notification data.

---

### Key Code Snippets

#### Class: `VesselVisitNotificationController`

```c#
[HttpPut("{id}/update")]
        public async Task<ActionResult<VesselVisitNotificationDto>> UpdateInProgress(Guid id, [FromBody] UpdateNotificationDto dto)
        {
            try
            {
                var updated = await _service.UpdateInProgressAsync(id, dto.LoadingCargo, dto.UnloadingCargo);
                return Ok(updated);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        [HttpPut("{id}/submit")]
        public async Task<ActionResult<VesselVisitNotificationDto>> Submit(Guid id)
        {
            try
            {
                var submitted = await _service.SubmitForApprovalAsync(id);
                return Ok(submitted);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
```
#### Class: `VesselVisitNotificationService``

```c#
 public async Task<VesselVisitNotificationDto> UpdateInProgressAsync(Guid id, LoadingCargoMaterial newLoading, UnloadingCargoMaterial newUnloading)
        {
            var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));

            if (notification == null)
                throw new BusinessRuleValidationException("Vessel Visit Notification not found.");

            // Se está a atualizar unloading cargo, validar capacidade
            if (newUnloading != null)
            {
                var vesselType = await _vesselTypeRepo.GetByIdAsync(notification.Vessel.VesselTypeId);
                if (vesselType == null)
                    throw new BusinessRuleValidationException("Vessel type not found.");

                var unloadingWeight = newUnloading.TotalWeightKg();
                var vesselCapacity = vesselType.Capacity;
                
                if (unloadingWeight > vesselCapacity)
                    throw new BusinessRuleValidationException(
                        $"Unloading cargo weight ({unloadingWeight} kg) cannot exceed vessel capacity ({vesselCapacity} kg).");
            }

            notification.UpdateInProgress(newLoading, newUnloading);

            await _unitOfWork.CommitAsync();
            return MapToDto(notification);
        }
        
````

#### Class: `UpdateNotificationDto`
```c#
public class UpdateNotificationDto
    {
        public LoadingCargoMaterial LoadingCargo { get; set; }
        public UnloadingCargoMaterial UnloadingCargo { get; set; }
    }
````