### 5.2. Description
### 5.3. Implementation Details
# 5. User Story Implementation Report

---

## 5.1. US_2.2.13 – Register and Manage Qualifications

### 5.2. Description

As a Logistics Operator, I want to register and manage qualifications (create, update, list, delete) so staff and resources can reference valid certifications required for operations.

### 5.3. Implementation Details

- User interaction & endpoints
  - REST API in `QualificationsController` provides: POST /api/Qualifications, GET /api/Qualifications, GET /api/Qualifications/{id}, PUT /api/Qualifications/{id}, DELETE /api/Qualifications/{id}, GET /api/Qualifications/search?name={name}, GET /api/Qualifications/exists/{id}.

- Business logic
  - `QualificationService` orchestrates operations: validates DTOs, constructs `Qualification` aggregates, calls `IQualificationRepository` and commits via `IUnitOfWork.CommitAsync()`.

- Domain
  - `Qualification` is the aggregate root. It enforces name validation rules (required, at least two words, max length 150). The aggregate exposes methods to change the name and to validate its invariants.

- Persistence
  - `IQualificationRepository` / `QualificationRepository` expose: `AddAsync`, `GetByIdAsync`, `UpdateAsync`, `Remove`, `GetAllAsync`, `GetByNameAsync`, and `ExistsAsync` (used by service and tests).

- Validation & business rules
  - Qualification name is validated on creation and update (see domain tests). Invalid names raise BusinessRuleValidationException.
  - Qualifications must exist before they are assigned to other aggregates (e.g., `StaffMemberService` checks `IQualificationRepository.GetByIdAsync`).

- Search & filter
  - Search supports partial name matching (used by integration tests: `/api/Qualifications/search?name=...`).

- Security
  - Endpoints intended for Logistics Operator role. Controller actions should be protected with `[Authorize(Roles = "LogisticsOperator")]` (the project registers `IQualificationRepository` and `QualificationService` in `Startup.cs`; authentication scheme should be configured for runtime/tests as needed).

- Integration with other entities
  - Qualifications are referenced by `StaffMember` (via `AddQualificationAsync` flow) and by resource entities. Referential integrity is enforced at service level before assignment.

---

### Key Code Snippets (representative)

#### Controller: selected endpoints

```csharp
[HttpPost]
public async Task<ActionResult<QualificationDto>> Create([FromBody] CreateQualificationDto dto)
{
    try
    {
        var created = await _service.CreateAsync(dto);
        return CreatedAtAction(nameof(GetById), new { id = created.Id }, created);
    }
    catch (BusinessRuleValidationException ex)
    {
        return BadRequest(new { Message = ex.Message });
    }
}

[HttpPut("{id}")]
public async Task<ActionResult<QualificationDto>> Update(Guid id, [FromBody] UpdateQualificationDto dto)
{
    try
    {
        var updated = await _service.UpdateAsync(id, dto);
        return Ok(updated);
    }
    catch (BusinessRuleValidationException ex)
    {
        return BadRequest(new { Message = ex.Message });
    }
}

[HttpGet("search")]
public async Task<ActionResult<IEnumerable<QualificationDto>>> Search([FromQuery] string name)
{
    var results = await _service.SearchByNameAsync(name);
    return Ok(results);
}
```

#### Service: representative methods (simplified)

```csharp
public async Task<QualificationDto> CreateAsync(CreateQualificationDto dto)
{
    var qual = new Qualification(dto.Name);
    await _repo.AddAsync(qual);
    await _unitOfWork.CommitAsync();
    return MapToDto(qual);
}

public async Task<QualificationDto> UpdateAsync(Guid id, UpdateQualificationDto dto)
{
    var qual = await _repo.GetByIdAsync(new QualificationID(id));
    if (qual == null) throw new BusinessRuleValidationException("Qualification not found.");
    qual.ChangeName(dto.Name);
    await _unitOfWork.CommitAsync();
    return MapToDto(qual);
}

public async Task<List<QualificationDto>> SearchByNameAsync(string name)
{
    var list = await _repo.GetByNameAsync(name);
    return list.Select(MapToDto).ToList();
}
```

#### Aggregate: `Qualification` (representative)

```csharp
public Qualification(string name)
{
    if (string.IsNullOrWhiteSpace(name)) throw new BusinessRuleValidationException("Qualification name is required.");
    if (name.Split(' ').Length < 2) throw new BusinessRuleValidationException("Qualification name must contain at least two words.");
    if (name.Length > 150) throw new BusinessRuleValidationException("Qualification name must have a maximum length of 150 characters.");

    Id = QualificationID.Generate();
    Name = name;
}

public void ChangeName(string newName)
{
    // same validations as constructor
    Name = newName;
}
```