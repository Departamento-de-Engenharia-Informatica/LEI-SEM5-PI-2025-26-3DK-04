# US 2.2.6 - Manage Shipping Agent Representatives

## Full Implementation

### Files Created/Modified

#### Domain Layer
* Domain/Representatives/Representative.cs
* Domain/Representatives/RepresentativeDto.cs
* Domain/Representatives/CreateRepresentativeDto.cs
* Domain/Representatives/IRepresentativeRepository.cs
* Domain/Representatives/RepresentativeService.cs

#### Infrastructure Layer
* Infrastructure/Representatives/RepresentativeRepository.cs
* Infrastructure/Representatives/RepresentativeEntityTypeConfiguration.cs
* Infrastructure/DDDSample1DbContext.cs — adds DbSet<Representative>

#### Controllers
* Controllers/RepresentativesController.cs

---

## How to Test

### Available Endpoints

#### **POST /api/Representatives**
Create a representative

```http
POST https://localhost:5001/api/Representatives
Content-Type: application/json

{
  "organizationId": "ORG-001",
  "name": "Maria Costa",
  "citizenId": "1122334455",
  "nationality": "PT",
  "email": "maria@atlantic.com",
  "phone": "+351934567890"
}
```

---

#### **GET /api/Representatives**
List all representatives

```http
GET https://localhost:5001/api/Representatives
```

---

#### **PUT /api/Representatives/{id}**
Update representative data

```http
PUT https://localhost:5001/api/Representatives/REP-001
Content-Type: application/json

{
  "email": "novoemail@atlantic.com",
  "phone": "+351999999999"
}
```

---

#### **PUT /api/Representatives/{id}/deactivate**
Deactivate a representative

```http
PUT https://localhost:5001/api/Representatives/REP-001/deactivate
```

---

#### **PUT /api/Representatives/{id}/activate**
Reactivate a representative

```http
PUT https://localhost:5001/api/Representatives/REP-001/activate
```

---

## Implemented Business Rules

* Each representative belongs to a single organization
* Deactivated representatives cannot authenticate in the system
* Email and CitizenId must be unique within the organization
* Updates trigger an automatic email notification

## Tests

* Create valid representative → 201 Created
* Update representative → 200 OK
* Deactivate representative → 200 OK, status = Inactive
* Reactivate representative → 200 OK, status = Active
* Create duplicate representative → 409 Conflict

---
