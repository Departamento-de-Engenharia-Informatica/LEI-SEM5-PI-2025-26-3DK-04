# 5. User Story Implementation Report

---

## 5.1. US_2.2.11 – Register and Manage Operating Staff Members

### 5.2. Description

As a Logistics Operator, I want to create, update and deactivate operating staff members so the system can track availability and qualifications.

---

### 5.3. Implementation Details

- User interaction & endpoints
  - REST API in `StaffMembersController` provides: POST /api/StaffMembers, GET /api/StaffMembers/{id}, PUT /api/StaffMembers/{id}, DELETE /api/StaffMembers/{id}, PUT /api/StaffMembers/{id}/reactivate, POST /api/StaffMembers/{id}/qualifications, DELETE /api/StaffMembers/{staffId}/qualifications/{qualificationId}, and search endpoints.

- Business logic
  - `StaffMemberService` orchestrates repository calls, uses `IQualificationRepository` for qualification lookups, and commits via `IUnitOfWork.CommitAsync()`.

- Domain
  - `StaffMember` is the aggregate root. It validates email/phone, manages qualifications, and implements `Change*`, `AddQualification`, `RemoveQualification`, `Deactivate` and `Reactivate` methods.

- Persistence
  - `IStaffMemberRepository` / `StaffMemberRepository` expose methods: `AddAsync`, `GetByIdAsync`, `GetActiveStaffAsync`, `GetAllForAuditAsync`, `GetByNameAsync`, `GetByStatusAsync`, `GetByQualificationAsync`, `SearchAsync`.

- Security
  - Controller endpoints are intended to be protected by role-based authorization (Logistics Operator). The actual middleware/attributes are defined in the controller (see code snippets below).

---

### Key Code Snippets

#### Controller: representative endpoints

```csharp
[HttpPost]
public async Task<ActionResult<StaffMemberDto>> Create([FromBody] CreateStaffMemberDto dto)
{
    try
    {
        var staff = await _service.CreateAsync(dto);
        return CreatedAtAction(nameof(GetById), new { id = staff.Id }, staff);
    }
    catch (BusinessRuleValidationException ex)
    {
        return BadRequest(new { Message = ex.Message });
    }
}

[HttpPut("{id}")]
public async Task<ActionResult<StaffMemberDto>> Update(Guid id, [FromBody] UpdateStaffMemberDto dto)
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

[HttpDelete("{id}")]
public async Task<ActionResult<StaffMemberDto>> Deactivate(Guid id)
{
    try
    {
        var result = await _service.DeactivateAsync(id);
        return Ok(result);
    }
    catch (BusinessRuleValidationException ex)
    {
        return BadRequest(new { Message = ex.Message });
    }
}
```

#### Service: selected methods (simplified)

```csharp
public async Task<StaffMemberDto> CreateAsync(CreateStaffMemberDto dto)
{
    var staff = new StaffMember(dto.Name, dto.Email, dto.PhoneNumber, dto.OperationalWindow);
    await _repo.AddAsync(staff);
    await _unitOfWork.CommitAsync();
    return MapToDto(staff);
}

public async Task<StaffMemberDto> UpdateAsync(Guid id, UpdateStaffMemberDto dto)
{
    var staff = await _repo.GetByIdAsync(new StaffMemberID(id));
    if (staff == null) throw new BusinessRuleValidationException("Staff member not found.");

    if (!string.IsNullOrWhiteSpace(dto.Name)) staff.ChangeName(dto.Name);
    if (!string.IsNullOrWhiteSpace(dto.Email)) staff.ChangeEmail(dto.Email);
    if (dto.PhoneNumber.HasValue) staff.ChangePhoneNumber(dto.PhoneNumber.Value);

    await _unitOfWork.CommitAsync();
    return MapToDto(staff);
}

public async Task<StaffMemberDto> AddQualificationAsync(Guid staffId, Guid qualificationId)
{
    var staff = await _repo.GetByIdAsync(new StaffMemberID(staffId));
    var qual = await _qualificationRepo.GetByIdAsync(new QualificationID(qualificationId));
    if (qual == null) throw new BusinessRuleValidationException("Qualification not found.");
    staff.AddQualification(qual);
    await _unitOfWork.CommitAsync();
    return MapToDto(staff);
}
```

#### Aggregate: representative methods

```csharp
public void AddQualification(Qualification qualification)
{
    if (qualification == null) throw new BusinessRuleValidationException("Qualification cannot be null.");
    if (Qualifications.Exists(q => q.Id == qualification.Id))
        throw new BusinessRuleValidationException("Qualification already exists.");
    Qualifications.Add(qualification);
}

public void Deactivate()
{
    if (Status == MemberStatus.Unavailable)
        throw new BusinessRuleValidationException("Staff member is already inactive.");
    Status = MemberStatus.Unavailable;
}
```