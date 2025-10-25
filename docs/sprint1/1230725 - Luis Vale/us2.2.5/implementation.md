
# 5. User Story Implementation Report

---

## 5.1. US_2.2.5 – Register Shipping Agent Organizations

### 5.2. Description

As a Port Authority Officer, I want to register new shipping agent organizations so that they can operate within the
port’s digital system.

### 5.3. Implementation Details
- **User Interaction & Flow Control**
 Yet to implement, since it is not required this sprint, using swagger as a substitute to test the API.

- **Business Logic Coordination**  
  The `OrganizationController` manages the registration process and delegates validation and persistence to the
  application service.

- **Domain Entity Construction**  
  The `Organization` entity encapsulates organization data and validation logic, while `Representative`
  stores contact and identification details.

- **Persistence**  
  The `IOrganizationRepository` interface and its implementation (`OrganizationRepository`) persist organization
  and representative data in the database.

---


---

### Key Code Snippets

#### Class: `OrganizationController`

```c#
[HttpPost]
        public async Task<ActionResult<OrganizationDto>> Register([FromBody] OrganizationDto dto)
        {
            try
            {
                var org = await _service.RegisterOrganizationAsync(dto);
                return CreatedAtAction(nameof(GetById), new { id = org.Id }, org);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }
```
#### Class: `Organization`

```c#
 public Organization(string id, string legalName, string alternativeName, string address, string taxNumber)
        {
            if (string.IsNullOrWhiteSpace(id))
                throw new BusinessRuleValidationException("Organization identifier is required.");

            if (string.IsNullOrWhiteSpace(legalName))
                throw new BusinessRuleValidationException("Legal name is required.");

            if (string.IsNullOrWhiteSpace(address))
                throw new BusinessRuleValidationException("Address is required.");

            // Validação do Tax Number europeu
            Validators.ValidateTaxNumber(taxNumber);

            this.Id = new OrganizationId(id); // O OrganizationId já valida o formato
            this.LegalName = legalName;
            this.AlternativeName = alternativeName;
            this.Address = address;
            this.TaxNumber = taxNumber;
        }
```

#### Class: `OrganizationService`

```c#
        public async Task<OrganizationDto> RegisterOrganizationAsync(OrganizationDto dto)
        {
            if (string.IsNullOrWhiteSpace(dto.Id))
                throw new BusinessRuleValidationException("Organization ID is required.");

            // Validar se já existe uma organização com este ID
            var existingById = await _repo.GetByIdAsync(new OrganizationId(dto.Id));
            if (existingById != null)
                throw new BusinessRuleValidationException($"An organization with ID '{dto.Id}' already exists.");

            if (await _repo.ExistsWithLegalNameAsync(dto.LegalName))
                throw new BusinessRuleValidationException("An organization with this legal name already exists.");

            var org = new Organization(dto.Id, dto.LegalName, dto.AlternativeName, dto.Address, dto.TaxNumber);


            if (dto.Representatives != null && dto.Representatives.Any())
            {
                foreach (var repDto in dto.Representatives)
                {
                    if (await _repRepo.ExistsWithEmailAsync(repDto.Email))
                        throw new BusinessRuleValidationException("Email already in use by another representative.");

                    if (await _repRepo.ExistsWithPhoneAsync(repDto.PhoneNumber))
                        throw new BusinessRuleValidationException("Phone number already in use by another representative.");
                    if(await _repRepo.ExistsWithCidAsync(repDto.CitizenId))
                        throw new BusinessRuleValidationException("Citizen Id already in use by another representative.");
                    var rep = new Representative(repDto.Name, repDto.CitizenId, repDto.Nationality, repDto.Email,
                        repDto.PhoneNumber);
                    org.AddRepresentative(rep);
                    rep.AssignToOrganization(org.Id);
                }
            }


            org.ValidateReadyForRegistration();

            await _repo.AddAsync(org);
            await _unitOfWork.CommitAsync();

            return ToDto(org);
        }
````

#### Class: `OrganizationRepository`

```c#
public async Task<Organization> GetByIdAsync(OrganizationId id)
        {
            return await _objs
                .Include(o => o.Representatives)
                .FirstOrDefaultAsync(o => o.Id.Equals(id));
        }
````

#### Class: `OrganizationDto`
```c#
 public class OrganizationDto
    {
        public string Id { get; set; }
        public string LegalName { get; set; }
        public string AlternativeName { get; set; }
        public string Address { get; set; }
        public string TaxNumber { get; set; }
        public List<AddRepresentativeToOrgDto> Representatives { get; set; }
    }
````