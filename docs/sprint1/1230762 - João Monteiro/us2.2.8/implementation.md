# 5. User Story Implementation Report

---

## 5.1 US_2.2.8 – Create/Submit Vessel Visit Notification

---

## 5.2 Description

As a **Shipping Agent Representative**, I want to create and submit a Vessel Visit Notification, so that the vessel berthing and subsequent (un)loading operations at the port are scheduled and planned in a spatially and timely manner.

---

## 5.3 Implementation Details

### User Interaction & Endpoints

- **POST** `/api/VesselVisitNotifications` — create a new vessel visit notification.
- **PUT** `/api/VesselVisitNotifications/{id}/cargo` — add or update cargo manifests for loading/unloading.
- **PUT** `/api/VesselVisitNotifications/{id}/crew` — add or update crew information when required.
- **PATCH** `/api/VesselVisitNotifications/{id}/status` — change status (InProgress or Submitted).
- **GET** `/api/VesselVisitNotifications/{id}` — retrieve notification details.
- **GET** `/api/VesselVisitNotifications` — list all notifications.

### Business Logic

The **VesselVisitNotificationService** orchestrates operations:

- Validates incoming DTOs for vessel, cargo, and crew.
- Enforces ISO 6346:2022 container validation for cargo manifests.
- Enforces aggregate rules for status transitions: **Draft → InProgress → Submitted**.
- Persists changes via **UnitOfWork**.

### Domain Entity

**VesselVisitNotification** is the aggregate root and encapsulates:

- **Vessel information:** IMO number, name, arrival/departure dates.
- **Cargo manifests:** for loading/unloading, with ISO 6346:2022 container validation.
- **Crew members:** name, citizen ID, nationality (when required).
- **Status:** Draft, InProgress, Submitted.
- **Business rules:** for creation, cargo/crew updates, and submission.

### Persistence

- **IVesselVisitNotificationRepository** provides retrieval, update, and add methods.
- Changes are persisted using `UnitOfWork.CommitAsync()`.

### Security

- Controller endpoints are protected with **role-based authorization** (Shipping Agent Representative).

### Key Code Snippets


#### Service: `VesselVisitNotificationController` (simplified)

```csharp
[HttpPost]
public async Task<ActionResult<VesselVisitNotificationDto>> Create(CreateVesselVisitNotificationDto dto)
{
    var notification = await _service.CreateAsync(dto);
    return CreatedAtAction(nameof(GetById), new { id = notification.Id }, notification);
}

[HttpPut("{id}/cargo")]
public async Task<ActionResult<VesselVisitNotificationDto>> AddOrUpdateCargo(string id, List<CargoManifestDto> cargo)
{
    var notification = await _service.AddOrUpdateCargoAsync(new VesselVisitNotificationID(id), cargo);
    return Ok(notification);
}

[HttpPut("{id}/crew")]
public async Task<ActionResult<VesselVisitNotificationDto>> AddOrUpdateCrew(string id, List<CrewMemberDto> crew)
{
    var notification = await _service.AddOrUpdateCrewAsync(new VesselVisitNotificationID(id), crew);
    return Ok(notification);
}

[HttpPatch("{id}/status")]
public async Task<ActionResult<VesselVisitNotificationDto>> ChangeStatus(string id, ChangeStatusDto dto)
{
    var notification = await _service.ChangeStatusAsync(new VesselVisitNotificationID(id), dto.Status);
    return Ok(notification);
}

[HttpGet("{id}")]
public async Task<ActionResult<VesselVisitNotificationDto>> GetById(string id)
{
    var notification = await _service.GetByIdAsync(new VesselVisitNotificationID(id));
    if (notification == null) return NotFound();
    return Ok(notification);
}

```

#### Aggregate: `VesselVisitNotification` (concept)

```csharp
public VesselVisitNotification(VesselInfo vessel, List<CargoManifest> cargo = null, List<CrewMember> crew = null)
{
    Id = new VesselVisitNotificationID(Guid.NewGuid());
    Vessel = vessel ?? throw new BusinessRuleValidationException("Vessel info is required.");
    CargoManifests = cargo ?? new List<CargoManifest>();
    CrewMembers = crew ?? new List<CrewMember>();
    Status = NotificationStatus.Draft;
}

public void UpdateCargoManifests(List<CargoManifest> cargo)
{
    foreach (var item in cargo)
    {
        if (!Iso6346Validator.IsValid(item.ContainerId))
            throw new BusinessRuleValidationException($"Invalid container ID: {item.ContainerId}");
    }
    CargoManifests = cargo;
}

public void UpdateCrewMembers(List<CrewMember> crew)
{
    if (crew.Any(c => string.IsNullOrWhiteSpace(c.Name) || string.IsNullOrWhiteSpace(c.CitizenId)))
        throw new BusinessRuleValidationException("Invalid crew member data.");
    CrewMembers = crew;
}

public void ChangeStatus(NotificationStatus newStatus)
{
    if (newStatus == NotificationStatus.InProgress && Status != NotificationStatus.Draft)
        throw new BusinessRuleValidationException("Can only move to InProgress from Draft.");

    if (newStatus == NotificationStatus.Submitted)
    {
        if (!CargoManifests.Any()) throw new BusinessRuleValidationException("Cannot submit without cargo manifests.");
        if (CargoManifests.Any(c => !Iso6346Validator.IsValid(c.ContainerId)))
            throw new BusinessRuleValidationException("Cannot submit: invalid container IDs.");
    }

    Status = newStatus;
}


```