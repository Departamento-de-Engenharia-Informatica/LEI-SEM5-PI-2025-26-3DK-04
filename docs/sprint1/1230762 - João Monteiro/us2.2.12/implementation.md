# 5. User Story Implementation Report

## 5.1 US_2.2.12 – Manage Physical Resources

---

## 5.2 Description

As a Port Authority Officer, I want to create, update, assign qualifications, change status, and search physical resources, so that resources can be managed efficiently with proper capacity, type, and qualifications.

---

## 5.3 Implementation Details

### User Interaction & Endpoints

- **POST /api/PhysicalResources** — create a new physical resource.
- **PUT /api/PhysicalResources/{id}** — update an existing resource.
- **PATCH /api/PhysicalResources/{id}/status** — change the status (Active/Inactive).
- **GET /api/PhysicalResources** — list resources with optional filters.
- **GET /api/PhysicalResources/{id}** — retrieve resource details.

### Business Logic

`PhysicalResourceService` handles:

- Validation of incoming DTOs.
- Resolves Qualification references.
- Enforces business rules in the aggregate (`PhysicalResource`).
- Commits changes via `UnitOfWork`.

### Domain Entity

`PhysicalResource` is the aggregate root and encapsulates:

- Description, type, capacity, assigned area.
- Setup time and status (Active/Inactive).
- Associated qualifications.
- Business rules for creation, updates, qualification assignment, and status changes.

### Persistence

- `IPhysicalResourceRepository` provides retrieval, update, and delete methods.
- Changes are persisted via `UnitOfWork.CommitAsync()`.

### Security

Controller endpoints are intended to be protected by role-based authorization (Port Authority Officer).

---

## Key Code Snippets

### Controller: PhysicalResourceController (create/update/status endpoints)

```csharp
[HttpPost]
public async Task<ActionResult<PhysicalResourceDto>> Create(CreatePhysicalResourceDto dto)
{
    var resource = await _service.AddAsync(dto);
    return CreatedAtAction(nameof(GetById), new { id = resource.Id }, resource);
}

[HttpPut("{id}")]
public async Task<ActionResult<PhysicalResourceDto>> Update(Guid id, UpdatePhysicalResourceDto dto)
{
    var resource = await _service.UpdateAsync(id, dto);
    if (resource == null) return NotFound();
    return Ok(resource);
}

[HttpPatch("{id}/status")]
public async Task<ActionResult<PhysicalResourceDto>> ChangeStatus(Guid id, ChangeStatusDto dto)
{
    var resource = await _service.ChangeStatusAsync(id, dto.NewStatus);
    if (resource == null) return NotFound();
    return Ok(resource);
}

```

#### Service: `PhysicalResourceService` (simplified)

```csharp
public async Task<PhysicalResourceDto> AddAsync(CreatePhysicalResourceDto dto)
{
    var qualifications = await _qualificationRepo.GetByIdsAsync(dto.QualificationIds);
    var resource = new PhysicalResource(dto.Description, dto.Type, dto.Capacity, dto.AssignedArea, dto.SetupTime, dto.Status, qualifications);

    await _repo.AddAsync(resource);
    await _unitOfWork.CommitAsync();

    return MapToDto(resource);
}

public async Task<PhysicalResourceDto> UpdateAsync(Guid id, UpdatePhysicalResourceDto dto)
{
    var resource = await _repo.GetByIdAsync(id);
    if (resource == null) return null;

    var qualifications = await _qualificationRepo.GetByIdsAsync(dto.QualificationIds);
    resource.Update(dto.Description, dto.Type, dto.Capacity, dto.AssignedArea, dto.SetupTime, qualifications);

    await _unitOfWork.CommitAsync();
    return MapToDto(resource);
}

public async Task<PhysicalResourceDto> ChangeStatusAsync(Guid id, ResourceStatus status)
{
    var resource = await _repo.GetByIdAsync(id);
    if (resource == null) return null;

    resource.ChangeStatus(status);
    await _unitOfWork.CommitAsync();
    return MapToDto(resource);
}


```

#### Aggregate: `PhysicalResource` (concept)

```csharp
public PhysicalResource(
    string description, 
    string type, 
    double capacity, 
    string? assignedArea, 
    int setupTime, 
    ResourceStatus status, 
    List<Qualification> qualifications)
{
    if (string.IsNullOrWhiteSpace(description)) throw new BusinessRuleValidationException("Description is required.");
    if (capacity <= 0) throw new BusinessRuleValidationException("Capacity must be greater than 0.");
    
    Id = new PhysicalResourceID(Guid.NewGuid());
    Description = description;
    Type = type;
    Capacity = capacity;
    AssignedArea = assignedArea;
    SetupTime = setupTime;
    Status = status;
    Qualifications = qualifications ?? new List<Qualification>();
}

public void Update(
    string description, 
    string type, 
    double capacity, 
    string? assignedArea, 
    int setupTime, 
    List<Qualification> qualifications)
{
    if (capacity <= 0) throw new BusinessRuleValidationException("Capacity must be greater than 0.");
    
    Description = description;
    Type = type;
    Capacity = capacity;
    AssignedArea = assignedArea;
    SetupTime = setupTime;
    Qualifications = qualifications;
}

public void ChangeStatus(ResourceStatus newStatus)
{
    Status = newStatus;
}


```