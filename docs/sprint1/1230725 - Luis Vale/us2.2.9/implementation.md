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
        public async Task<ActionResult<VesselVisitNotificationDto>> UpdateInProgress(string id, [FromBody] UpdateNotificationDto dto)
        {
            try
            {
                var updated = await _service.UpdateInProgressAsync(id, dto);
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
 public async Task<VesselVisitNotificationDto> UpdateInProgressAsync(string Id,UpdateNotificationDto dto)
        {
            if (string.IsNullOrWhiteSpace(dto.VesselId) && dto.LoadingCargo == null && dto.UnloadingCargo == null)
            {
                throw new BusinessRuleValidationException("At least one field (VesselId, LoadingCargo, or UnloadingCargo) must be provided for update.");
            }
            var notificationId = new VesselVisitNotificationID(Guid.Parse(Id));
            var notification = await _repo.GetByIdAsync(notificationId);

            if (notification == null)
                throw new BusinessRuleValidationException("Vessel Visit Notification not found.");

            if (notification.Status != NotificationStatus.InProgress)
                throw new BusinessRuleValidationException("Only notifications with 'InProgress' status can be updated.");
            
            Vessel vesselToUse = notification.Vessel;
            bool vesselChanged = false;

            if (!string.IsNullOrWhiteSpace(dto.VesselId))
            {
                var newVessel = await _vesselRepo.GetByIdAsync(new VesselId(Guid.Parse(dto.VesselId)));
                if (newVessel == null)
                    throw new BusinessRuleValidationException("Vessel not found.");
                vesselToUse = newVessel;
                vesselChanged = true;
            }
            
            LoadingCargoMaterial loadingToUse = notification.LoadingCargo;
            if (dto.LoadingCargo != null)
            {
                var manifests = dto.LoadingCargo.Manifests.Select(m =>
                {
                    var manifest = CargoManifest.Create(m.Id);
                    foreach (var container in m.Containers)
                        manifest.AddContainer(Container.Create(container.Id, container.PayloadWeight, container.ContentsDescription));
                    return manifest;
                }).ToList();

                loadingToUse = new LoadingCargoMaterial(manifests);
            }
            
            UnloadingCargoMaterial unloadingToUse = notification.UnloadingCargo;
            if (dto.UnloadingCargo != null)
            {
                var manifests = dto.UnloadingCargo.Manifests.Select(m =>
                {
                    var manifest = CargoManifest.Create(m.Id);
                    foreach (var container in m.Containers)
                        manifest.AddContainer(Container.Create(container.Id, container.PayloadWeight, container.ContentsDescription));
                    return manifest;
                }).ToList();

                unloadingToUse = new UnloadingCargoMaterial(manifests);
            }
            
            var vesselType = await _vesselTypeRepo.GetByIdAsync(vesselToUse.VesselTypeId);
            if (vesselType == null)
                throw new BusinessRuleValidationException("Vessel type not found for the specified vessel.");

            double totalWeight = loadingToUse.TotalWeightKg() + unloadingToUse.TotalWeightKg();

            if (totalWeight > vesselType.Capacity)
                throw new BusinessRuleValidationException(
                    $"Total cargo weight ({totalWeight} kg) exceeds vessel capacity ({vesselType.Capacity} kg).");
            
            if (vesselChanged)
                notification.UpdateVessel(vesselToUse);

            if (dto.LoadingCargo != null)
                notification.UpdateLoadingCargo(loadingToUse);

            if (dto.UnloadingCargo != null)
                notification.UpdateUnloadingCargo(unloadingToUse);

            await _unitOfWork.CommitAsync();

            return MapToDto(notification);
        }
````

#### Class: `UpdateNotificationDto`
```c#
public class UpdateNotificationDto
    {
        public string? VesselId { get; set; } 
        public LoadingCargoMaterialDto? LoadingCargo { get; set; }
        public UnloadingCargoMaterialDTO? UnloadingCargo { get; set; }
    }

````