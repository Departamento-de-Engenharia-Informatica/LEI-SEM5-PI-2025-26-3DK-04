# 5. User Story Implementation Report

---

## 5.1. US_2.2.3 – Create/Update Docks

### 5.2. Description

As a Port Authority Officer, I want to create and update docks, so that vessels can be assigned to proper docking locations with valid dimensions and allowed vessel types.

---

### 5.3. Implementation Details

- **User interaction & endpoints**
    - `POST /api/Dock` — create a new dock.
    - `PUT /api/Dock/{id}` — update an existing dock.
    - `DELETE /api/Dock/{id}` — soft delete (mark as inactive).
    - `DELETE /api/Dock/{id}/hard` — hard delete from the database.
    - `GET /api/Dock/{id}` — retrieve dock details.
    - `GET /api/Dock` — list all docks.

- **Business logic**
    - `DockService` implements application orchestration: validates incoming `DockDto`, resolves `VesselType` references, enforces business rules in the aggregate (`Dock`), and commits via `UnitOfWork`.

- **Domain entity**
    - `Dock` is the aggregate root and encapsulates:
        - Dock dimensions: length, depth, max draft.
        - Location and coordinates.
        - Allowed vessel types.
        - Active/inactive status.
        - Business rules for creation, updates, and state transitions.

- **Persistence**
    - `IDockRepository` provides retrieval and update methods.
    - Changes are persisted via `UnitOfWork.CommitAsync()`.

- **Security**
    - Controller endpoints are intended to be protected by role-based authorization (Port Authority Officer).

---

### Key Code Snippets

#### Controller: `DockController` (create/update endpoints)

```csharp
[HttpPost]
public async Task<ActionResult<DockDetailsDto>> Create(DockDto dto)
{
    try
    {
        var dock = await _service.AddAsync(dto);
        return CreatedAtAction(nameof(GetById), new { id = dock.Id }, dock);
    }
    catch (BusinessRuleValidationException ex)
    {
        return BadRequest(new { Message = ex.Message });
    }
}

[HttpPut("{id}")]
public async Task<ActionResult<DockDetailsDto>> Update(string id, DockDto dto)
{
    try
    {
        var dock = await _service.UpdateAsync(id, dto);
        if (dock == null) return NotFound();
        return Ok(dock);
    }
    catch (BusinessRuleValidationException ex)
    {
        return BadRequest(new { Message = ex.Message });
    }
}

[HttpDelete("{id}")]
public async Task<ActionResult<DockDetailsDto>> SoftDelete(string id)
{
    var dock = await _service.InactivateAsync(new DockID(id));
    if (dock == null) return NotFound();
    return Ok(dock);
}

[HttpDelete("{id}/hard")]
public async Task<ActionResult<DockDetailsDto>> HardDelete(string id)
{
    try
    {
        var dock = await _service.DeleteAsync(new DockID(id));
        if (dock == null) return NotFound();
        return Ok(dock);
    }
    catch (BusinessRuleValidationException ex)
    {
        return BadRequest(new { Message = ex.Message });
    }
}

```

#### Service: `DockService` (simplified)

```csharp
public async Task<DockDetailsDto> AddAsync(DockDto dto)
{
    var vesselTypes = await _vesselTypeRepo.GetByIdsAsync(dto.VesselTypeIds);
    var dock = new Dock(dto.Name, dto.Length, dto.Depth, dto.MaxDraft, new Location(dto.Coordinates, dto.LocationDescription), vesselTypes);
    
    await _repo.AddAsync(dock);
    await _unitOfWork.CommitAsync();

    return MapToDto(dock);
}

public async Task<DockDetailsDto> UpdateAsync(string id, DockDto dto)
{
    var dock = await _repo.GetByIdAsync(new DockID(id));
    if (dock == null) return null;

    var vesselTypes = await _vesselTypeRepo.GetByIdsAsync(dto.VesselTypeIds);
    dock.Update(dto.Name, dto.Length, dto.Depth, dto.MaxDraft, new Location(dto.Coordinates, dto.LocationDescription), vesselTypes);

    await _unitOfWork.CommitAsync();
    return MapToDto(dock);
}

public async Task<DockDetailsDto> InactivateAsync(DockID id)
{
    var dock = await _repo.GetByIdAsync(id);
    if (dock == null) return null;

    dock.MarkAsInactive();
    await _unitOfWork.CommitAsync();
    return MapToDto(dock);
}

public async Task<DockDetailsDto> DeleteAsync(DockID id)
{
    var dock = await _repo.GetByIdAsync(id);
    if (dock == null) return null;

    await _repo.RemoveAsync(dock);
    await _unitOfWork.CommitAsync();
    return MapToDto(dock);
}

```

#### Aggregate: `Dock` (concept)

```csharp
public Dock(string name, double length, double depth, int maxDraft, Location location, List<VesselType> vesselTypes)
{
    if (string.IsNullOrWhiteSpace(name)) throw new BusinessRuleValidationException("Dock name is required.");
    if (vesselTypes == null || !vesselTypes.Any()) throw new BusinessRuleValidationException("At least one vessel type must be assigned.");
    if (length <= 0) throw new BusinessRuleValidationException("Dock length must be greater than 0.");
    if (depth <= 0) throw new BusinessRuleValidationException("Dock depth must be greater than 0.");
    if (maxDraft <= 0) throw new BusinessRuleValidationException("Dock max draft must be greater than 0.");

    Id = new DockID(Guid.NewGuid());
    Name = name;
    Length = length;
    Depth = depth;
    MaxDraft = maxDraft;
    Location = location;
    _allowedVesselTypes = vesselTypes;
}

public void Update(string name, double length, double depth, int maxDraft, Location location, List<VesselType> vesselTypes)
{
    if (length <= 0) throw new BusinessRuleValidationException("Dock length must be greater than 0.");
    if (depth <= 0) throw new BusinessRuleValidationException("Dock depth must be greater than 0.");
    if (maxDraft <= 0) throw new BusinessRuleValidationException("Dock max draft must be greater than 0.");

    Name = name;
    Length = length;
    Depth = depth;
    MaxDraft = maxDraft;
    Location = location;
    _allowedVesselTypes.Clear();
    _allowedVesselTypes.AddRange(vesselTypes);
}

public void MarkAsInactive()
{
    if (!Active) throw new BusinessRuleValidationException("Dock is already inactive.");
    Active = false;
}

```