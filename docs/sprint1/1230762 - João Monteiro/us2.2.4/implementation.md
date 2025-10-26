# 5. User Story Implementation Report

---

## 5.1. US_2.2.4 – Create/Update/Inactivate/Activate Storage Areas

### 5.2. Description

As a Port Authority Officer, I want to create, update, inactivate, and activate storage areas, so that containers can be stored efficiently, and docks can be assigned dynamically.

---

### 5.3. Implementation Details

- **User interaction & endpoints**
    - `POST /api/StorageArea` — create a new storage area.
    - `PUT /api/StorageArea/{id}` — update an existing storage area.
    - `PATCH /api/StorageArea/{id}/inactivate` — mark a storage area as inactive.
    - `PATCH /api/StorageArea/{id}/activate` — mark a storage area as active.
    - `POST /api/StorageArea/{id}/assignDock` — assign a dock to the storage area.
    - `DELETE /api/StorageArea/{id}/unassignDock/{dockId}` — remove a dock assignment.
    - `GET /api/StorageArea/{id}` — retrieve storage area details.
    - `GET /api/StorageArea` — list all storage areas.

- **Business logic**
    - `StorageAreaService` orchestrates storage area operations: validates incoming DTOs, resolves dock references, enforces aggregate rules, and commits changes via `UnitOfWork`.

- **Domain entity**
    - `StorageArea` is the aggregate root and encapsulates:
        - Storage area code, designation, type (Refrigerated, Yard, etc.).
        - Location, coordinates, and location description.
        - Maximum and current capacity in TEUs.
        - Assigned docks with distances.
        - Active/inactive status.
        - Business rules for creation, updates, assignments, and status changes.

- **Persistence**
    - `IStorageAreaRepository` provides retrieval and update methods.
    - Changes are persisted via `UnitOfWork.CommitAsync()`.

- **Security**
    - Controller endpoints are intended to be protected by role-based authorization (Port Authority Officer).

---

### Key Code Snippets

#### Controller: `StorageAreaController` (create/update/assign/inactivate endpoints)

```csharp
[HttpPost]
public async Task<ActionResult<StorageAreaDto>> Create(CreateStorageAreaDto dto)
{
    var area = await _service.AddAsync(dto);
    return CreatedAtAction(nameof(GetById), new { id = area.Id }, area);
}

[HttpPut("{id}")]
public async Task<ActionResult<StorageAreaDto>> Update(string id, UpdateStorageAreaDto dto)
{
    var area = await _service.UpdateAsync(id, dto);
    if (area == null) return NotFound();
    return Ok(area);
}

[HttpPatch("{id}/inactivate")]
public async Task<ActionResult<StorageAreaDto>> Inactivate(string id)
{
    var area = await _service.InactivateAsync(new StorageAreaID(id));
    if (area == null) return NotFound();
    return Ok(area);
}

[HttpPatch("{id}/activate")]
public async Task<ActionResult<StorageAreaDto>> Activate(string id)
{
    var area = await _service.ActivateAsync(new StorageAreaID(id));
    if (area == null) return NotFound();
    return Ok(area);
}

[HttpPost("{id}/assignDock")]
public async Task<ActionResult<StorageAreaDto>> AssignDock(string id, AssignDockDto dto)
{
    var area = await _service.AssignDockAsync(new StorageAreaID(id), dto.DockId, dto.DistanceMeters);
    return Ok(area);
}

[HttpDelete("{id}/unassignDock/{dockId}")]
public async Task<ActionResult<StorageAreaDto>> UnassignDock(string id, Guid dockId)
{
    var area = await _service.UnassignDockAsync(new StorageAreaID(id), dockId);
    return Ok(area);
}

```

#### Service: `StorageAreaService` (simplified)

```csharp
public async Task<StorageAreaDto> AddAsync(CreateStorageAreaDto dto)
{
    var area = new StorageArea(dto.Code, dto.Designation, dto.StorageAreaType, 
                               new Location(dto.Coordinates, dto.LocationDescription),
                               dto.MaxCapacityTEUs, dto.InitialDockAssignments);
    
    await _repo.AddAsync(area);
    await _unitOfWork.CommitAsync();
    return MapToDto(area);
}

public async Task<StorageAreaDto> UpdateAsync(string id, UpdateStorageAreaDto dto)
{
    var area = await _repo.GetByIdAsync(new StorageAreaID(id));
    if (area == null) return null;

    area.Update(dto.Code, dto.Designation, dto.StorageAreaType,
                new Location(dto.Coordinates, dto.LocationDescription),
                dto.MaxCapacityTEUs, dto.CurrentOccupancyTEUs);
    
    await _unitOfWork.CommitAsync();
    return MapToDto(area);
}

public async Task<StorageAreaDto> AssignDockAsync(StorageAreaID id, Guid dockId, int distance)
{
    var area = await _repo.GetByIdAsync(id);
    area.AssignDock(dockId, distance);
    await _unitOfWork.CommitAsync();
    return MapToDto(area);
}

public async Task<StorageAreaDto> UnassignDockAsync(StorageAreaID id, Guid dockId)
{
    var area = await _repo.GetByIdAsync(id);
    area.UnassignDock(dockId);
    await _unitOfWork.CommitAsync();
    return MapToDto(area);
}

public async Task<StorageAreaDto> InactivateAsync(StorageAreaID id)
{
    var area = await _repo.GetByIdAsync(id);
    area.MarkAsInactive();
    await _unitOfWork.CommitAsync();
    return MapToDto(area);
}

public async Task<StorageAreaDto> ActivateAsync(StorageAreaID id)
{
    var area = await _repo.GetByIdAsync(id);
    area.MarkAsActive();
    await _unitOfWork.CommitAsync();
    return MapToDto(area);
}

```

#### Aggregate: `StorageArea` (concept)

```csharp
public StorageArea(string code, string designation, StorageAreaType type, Location location,
                   int maxCapacityTEUs, List<DockAssignment> initialDocks = null)
{
    if (string.IsNullOrWhiteSpace(code)) throw new BusinessRuleValidationException("Code is required.");
    if (maxCapacityTEUs <= 0) throw new BusinessRuleValidationException("Max capacity must be greater than 0.");
    
    Id = new StorageAreaID(Guid.NewGuid());
    Code = code;
    Designation = designation;
    StorageAreaType = type;
    Location = location;
    MaxCapacityTEUs = maxCapacityTEUs;
    CurrentOccupancyTEUs = 0;
    AssignedDocks = initialDocks ?? new List<DockAssignment>();
}

public void Update(string code, string designation, StorageAreaType type, Location location,
                   int maxCapacityTEUs, int currentOccupancyTEUs)
{
    if (maxCapacityTEUs <= 0) throw new BusinessRuleValidationException("Max capacity must be greater than 0.");
    
    Code = code;
    Designation = designation;
    StorageAreaType = type;
    Location = location;
    MaxCapacityTEUs = maxCapacityTEUs;
    CurrentOccupancyTEUs = currentOccupancyTEUs;
}

public void AssignDock(Guid dockId, int distance)
{
    if (AssignedDocks.Any(d => d.DockId == dockId))
        throw new BusinessRuleValidationException("Dock already assigned.");
    AssignedDocks.Add(new DockAssignment(dockId, distance));
}

public void UnassignDock(Guid dockId)
{
    var dock = AssignedDocks.FirstOrDefault(d => d.DockId == dockId);
    if (dock != null) AssignedDocks.Remove(dock);
}

public void MarkAsInactive()
{
    if (!Active) throw new BusinessRuleValidationException("Storage area already inactive.");
    Active = false;
}

public void MarkAsActive()
{
    if (Active) throw new BusinessRuleValidationException("Storage area already active.");
    Active = true;
}


```