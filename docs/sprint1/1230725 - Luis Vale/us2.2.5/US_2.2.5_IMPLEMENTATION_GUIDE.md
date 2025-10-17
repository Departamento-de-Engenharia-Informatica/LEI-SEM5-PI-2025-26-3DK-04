# US 2.2.5 - Register New Shipping Agent Organization

##  Full Implementation

### Files Created/Modified

#### Domain Layer
* Domain/Organizations/Organization.cs — Base entity with core properties (Identifier, LegalName, AlternativeName, Address, TaxNumber)
* Domain/Organizations/OrganizationDto.cs — Response DTO
* Domain/Organizations/CreateOrganizationDto.cs — Creation DTO
* Domain/Organizations/IOrganizationRepository.cs — Repository interface
* Domain/Organizations/OrganizationService.cs — Organization creation and validation logic

#### Infrastructure Layer
* Infrastructure/Organizations/OrganizationRepository.cs — Repository implementation
* Infrastructure/Organizations/OrganizationEntityTypeConfiguration.cs — EF configuration
* Infrastructure/DDDSample1DbContext.cs — Added DbSet<Organization>

#### Controllers
* Controllers/OrganizationsController.cs — REST API endpoints

#### Configuration
* Startup.cs — Dependency injection for OrganizationService and Repository

---

##  How to Test

### Run the Application
```bash
dotnet run
```


Swagger is available at:  
https://localhost:5001/swagger

---

### Available Endpoints

#### **GET /api/Organizations**
List all organizations

```http
GET https://localhost:5001/api/Organizations

```

**Response 200 OK:**
```json
[
  {
    "id": "ORG-001",
    "legalName": "Atlantic Shipping Ltd",
    "alternativeName": "Atlantic",
    "address": "Rua do Porto 100, Lisboa",
    "taxNumber": "PT500123456",
    "representatives": [
      {
        "id": "REP-001",
        "name": "João Silva",
        "email": "joao@atlantic.com",
        "phone": "+351912345678"
      }
    ]
  }
]
```

---

#### **POST /api/Organizations**
Register a new organization

```http
POST https://localhost:5001/api/Organizations
Content-Type: application/json

{
  "identifier": "ATL-SH-001",
  "legalName": "Atlantic Shipping Ltd",
  "alternativeName": "Atlantic",
  "address": "Rua do Porto 100, Lisboa",
  "taxNumber": "PT500123456",
  "representatives": [
    {
      "name": "João Silva",
      "citizenId": "987654321",
      "nationality": "PT",
      "email": "joao@atlantic.com",
      "phone": "+351912345678"
    }
  ]
}
```

**Response 201 Created:**
```json
{
  "id": "ORG-001",
  "legalName": "Atlantic Shipping Ltd",
  "representativesCount": 1
}
```

**Response 400 Bad Request:**
```json
{
  "message": "Organization must include at least one representative."
}
```

---

#### **GET /api/Organizations/{id}**
Get organization details

```http
GET https://localhost:5001/api/Organizations/ORG-001
```

---

## Implemented Business Rules

* Each organization must have at least one active representative
* Identifier and TaxNumber must be unique
* Representative email and phone number must be unique
* Automatic notifications are sent to representatives upon registration

---

## Tests

* Create organization with representative → 201 Created
* Create organization without representative → 400 Bad Request
* Duplicate taxNumber → 409 Conflict

---



