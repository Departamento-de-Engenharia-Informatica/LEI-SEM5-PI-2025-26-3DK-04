# 5. User Story Implementation Report

---

## 5.1. US_2.2.7 – Review Pending Vessel Visit Notifications

### 5.2. Description

As a Port Authority Officer, I want to review pending Vessel Visit Notifications and approve or reject them, so that docking schedules remain under port control.

---
### 5.3. Implementation Details

- User interaction & endpoints
  - GET /api/VesselVisitNotifications/completed — list notifications ready for review.
  - PUT /api/VesselVisitNotifications/{id}/approve — approve a notification and assign a dock.
  - PUT /api/VesselVisitNotifications/{id}/reject — reject a notification and record a reason.

- Business logic
  - `VesselVisitNotificationService` implements application orchestration: loads the aggregate from the repository, enforces business rules, invokes aggregate methods (`Approve` / `Reject`), and commits via Unit of Work.

- Domain entity
  - `VesselVisitNotification` is the aggregate root and encapsulates state transitions (approved/rejected), assigned dock, rejection reason, and decision metadata (timestamp, officerId, outcome).

- Persistence
  - `IVesselVisitNotificationRepository` provides retrieval and update methods; changes are persisted through `UnitOfWork.CommitAsync()`.

- Security
  - Controller endpoints are intended to be protected by role-based authorization (Port Authority Officer). The actual middleware/attributes are defined in the controller (see code snippets below).

---

### Key Code Snippets

#### Controller: `VesselVisitNotificationsController` (review endpoints)

```csharp
[HttpGet("completed")]
public async Task<ActionResult<IEnumerable<VesselVisitNotificationDto>>> GetCompletedNotifications()
{
    var dtos = await _service.GetCompletedNotificationsAsync();
    return Ok(dtos);
}

[HttpPut("{id}/approve")]
public async Task<IActionResult> Approve(Guid id, [FromBody] ApproveNotificationDto dto)
{
    try
    {
        var result = await _service.ApproveAsync(id, dto.OfficerId, dto.AssignedDock);
        return Ok(result);
    }
    catch (BusinessRuleValidationException ex)
    {
        return BadRequest(new { Message = ex.Message });
    }
}

[HttpPut("{id}/reject")]
public async Task<IActionResult> Reject(Guid id, [FromBody] RejectNotificationDto dto)
{
    try
    {
        var result = await _service.RejectAsync(id, dto.OfficerId, dto.Reason);
        return Ok(result);
    }
    catch (BusinessRuleValidationException ex)
    {
        return BadRequest(new { Message = ex.Message });
    }
}
```

#### Service: `VesselVisitNotificationService` (simplified)

```csharp
public async Task<VesselVisitNotificationDto> ApproveAsync(Guid id, Guid officerId, string assignedDock)
{
    var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));
    if (notification == null)
        throw new BusinessRuleValidationException("Notification not found.");

    // domain validation lives inside the aggregate
    notification.Approve(officerId, assignedDock);

    await _unitOfWork.CommitAsync();
    return MapToDto(notification);
}

public async Task<VesselVisitNotificationDto> RejectAsync(Guid id, Guid officerId, string reason)
{
    var notification = await _repo.GetByIdAsync(new VesselVisitNotificationID(id));
    if (notification == null)
        throw new BusinessRuleValidationException("Notification not found.");

    notification.Reject(officerId, reason);

    await _unitOfWork.CommitAsync();
    return MapToDto(notification);
}
```

#### Aggregate: `VesselVisitNotification` (concept)

```csharp
public void Approve(Guid officerId, string assignedDock)
{
    if (Status != NotificationStatus.Completed)
        throw new BusinessRuleValidationException("Only completed notifications can be approved.");

    AssignedDock = assignedDock;
    Status = NotificationStatus.Approved;
    DecisionTimestamp = DateTime.UtcNow;
    DecisionOfficerId = officerId;
}

public void Reject(Guid officerId, string reason)
{
    if (Status != NotificationStatus.Completed)
        throw new BusinessRuleValidationException("Only completed notifications can be rejected.");

    RejectionReason = reason;
    Status = NotificationStatus.Rejected;
    DecisionTimestamp = DateTime.UtcNow;
    DecisionOfficerId = officerId;
}
```