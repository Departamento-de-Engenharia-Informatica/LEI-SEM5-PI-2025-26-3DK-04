# 5. User Story Implementation Report

## 5.1. US_2.2.6 – Register and Manage Representatives

### 5.2. Description

As a Port Authority Officer, I want to manage representatives of shipping agent organizations (create, update,
deactivate) so that only authorized users can operate on behalf of their organizations.

### 5.3. Implementation Details

- **User Interaction & Flow Control**
  Yet to implement, since it is not required this sprint, using swagger as a substitute to test the API.
- **Business Logic:**  
  `RepresentativeService` validates data and coordinates repository and notification services.
- **Domain Entity Construction**
  `Representative` entity stores contact and identification details.
- **Persistence:**  
  `IRepresentativeRepository` persists representative data; inactive users are not removed but flagged.

---


### Key Code Snippets

#### Class: `RepresentativeController`

```c#
 [HttpPost]
        public async Task<ActionResult<RepresentativeDto>> AddRepresentative([FromBody] AddRepresentativeDto dto)
        {
            try
            {
                var rep = await _service.AddRepresentativeAsync(dto);
                return Ok(rep);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // PUT: api/Representatives/{id}/update
        [HttpPut("{id}/update")]
        public async Task<ActionResult<RepresentativeDto>> UpdateRepresentative([FromRoute] string id, [FromBody] UpdateRepresentativeDto dto)
        {
            try
            {
                var rep = await _service.UpdateRepresentativeAsync(id, dto);
                return Ok(rep);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
```
#### Class: `Representative`

```c#
 public Representative(string name, string citizenId, string nationality, string email, string phoneNumber)
        {
            if (string.IsNullOrEmpty(name))
                throw new BusinessRuleValidationException("Name is required.");
            
            if (string.IsNullOrWhiteSpace(citizenId))
                throw new BusinessRuleValidationException("Citizen ID is required.");

            // validações (email, phone, nationality)
            if (string.IsNullOrWhiteSpace(nationality))
                throw new BusinessRuleValidationException("Nationality is required.");
            if (string.IsNullOrWhiteSpace(email))
                throw new BusinessRuleValidationException("Email is required.");
            Validators.ValidateEmail(email);
            if (string.IsNullOrWhiteSpace(phoneNumber))
                throw new BusinessRuleValidationException("Phone number is required.");
            Validators.ValidatePhoneNumber(phoneNumber);           

            this.Id = new RepresentativeId(citizenId);
            this.Name = name;
            this.Nationality = nationality;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.Status = RepresentativeStatus.Active; // default
        }
```

#### Class: `RepresentativeService`

```c#
  public async Task<RepresentativeDto> AddRepresentativeAsync(AddRepresentativeDto dto)
        {
            if (await _repo.ExistsWithEmailAsync(dto.Email))
                throw new BusinessRuleValidationException("Email already in use by another representative.");
            if (await _repo.ExistsWithPhoneAsync(dto.PhoneNumber))
                throw new BusinessRuleValidationException("Phone number already in use by another representative.");
            if (await _repo.ExistsWithCidAsync(dto.CitizenId))
                throw new BusinessRuleValidationException("Citizen Id already in use by another representative.");

            var rep = new Representative(
                dto.Name,
                dto.CitizenId,
                dto.Nationality,
                dto.Email,
                dto.PhoneNumber
            );

            if (!string.IsNullOrWhiteSpace(dto.OrganizationId))
            {
                var org = await _organizationRepo.GetByIdAsync(new OrganizationId(dto.OrganizationId));
                if (org == null)
                    throw new BusinessRuleValidationException("Organization not found.");

                // 1. Atribuir OrganizationId imediatamente
                rep.AssignToOrganization(org.Id);

                // 2. Adicionar à organização
                org.AddRepresentative(rep);
            }
            else
            {
                throw new BusinessRuleValidationException("Representative must be assigned to an organization.");
            }

            await _repo.AddAsync(rep);
            await _unitOfWork.CommitAsync();

            return ToDto(rep);
        }

````

#### Class: `RepresentativeRepository`

```c#
public async Task<List<Representative>> GetActiveRepresentativesAsync()
        {
            return await _objs
                .Where(r => r.Status == RepresentativeStatus.Active)
                .ToListAsync();
        }
````

#### Class: `RepresentativeDto`
```c#
 public class RepresentativeDto
    {
      
        public string Name { get; set; }
        public string CitizenId { get; set; }
        public string Nationality { get; set; }

        public string Email { get; set; }

        public string PhoneNumber { get; set; }
        public string OrganizationId { get; set; }
        public string Status { get; set; }
    }
````