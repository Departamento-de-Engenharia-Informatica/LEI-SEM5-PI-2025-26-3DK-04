# 4. Unit, Integration, and System Tests

**User Story:** US2.2.12 – Manage Physical Resources

---

## 4.1 Unit Tests – PhysicalResourceTests.cs

**Purpose:**  
Ensure that the `PhysicalResource` aggregate enforces business rules for creation, updates, qualification assignment, and status changes.

**Test Descriptions:**

- **CreatePhysicalResource_WithValidParameters_Succeeds:**  
  Creating a resource with valid description, type, capacity, assigned area, and qualifications succeeds; aggregate stores values correctly.

- **CreatePhysicalResource_WithoutDescription_Throws:**  
  Description is required; if null or empty, `BusinessRuleValidationException` is thrown.

- **CreatePhysicalResource_WithInvalidCapacity_Throws:**  
  Capacity must be positive; otherwise, `BusinessRuleValidationException` is thrown.

- **UpdatePhysicalResource_WithValidParameters_Succeeds:**  
  Updating a resource with valid new values correctly updates all fields, including assigned area and setup time.

- **AssignQualification_ToResource_Succeeds:**  
  Adding a new qualification to a resource updates its `QualificationIds` correctly.

- **ChangeStatus_ActiveToInactiveAndBack_Succeeds:**  
  Changing the status between Active and Inactive updates the resource correctly; invalid transitions throw exceptions if any rules exist.

**Validation Criteria:**

- Domain rules throw exceptions on invalid operations.
- Resource status changes and qualification assignments are persisted correctly.
- Capacity and setup time constraints enforced.

**Expected Outcome:**  
All unit tests pass, ensuring the aggregate enforces creation, update, qualification, and status rules correctly.

---

## 4.2 Integration Tests – PhysicalResourceIntegrationTests.cs

**Purpose:**  
Verify correct interaction between controller, service, and repository layers for creating, updating, changing status, and searching physical resources.

**Test Descriptions:**

- **CreatePhysicalResource_ShouldReturnCreatedResource:**  
  `POST /api/PhysicalResources` → expect 201 Created, returned DTO contains correct values and assigned qualification IDs.

- **UpdatePhysicalResource_ShouldModifyResource:**  
  `PUT /api/PhysicalResources/{id}` → returns 200 OK, resource fields updated correctly.

- **ChangeStatus_ShouldUpdateStatus:**  
  `PATCH /api/PhysicalResources/{id}/status` → status updated to Inactive, then back to Active; returns 200 OK.

- **SearchPhysicalResources_ShouldFilterCorrectly:**  
  `GET /api/PhysicalResources?type=Vehicle` → returns only resources matching filter criteria; returned DTOs contain correct fields.

**Validation Criteria:**

- Correct HTTP status codes: 201 Created, 200 OK.
- Data persistence consistent: creation, update, status change reflected in database.
- Filters applied correctly in search results.

**Expected Outcome:**  
Integration tests confirm that the API endpoints orchestrate creation, updates, status changes, and searches correctly.

---

## 4.3 System Tests – PhysicalResourceSystemTests.cs

**Purpose:**  
Test end-to-end workflows via HTTP API, simulating real user actions for the physical resource lifecycle.

**Test Descriptions:**

- **CreatePhysicalResource_FullWorkflow_System:**  
  Create qualifications, then create a physical resource → expect 201 Created, returned DTO contains all correct fields.

- **UpdatePhysicalResource_FullWorkflow_System:**  
  After creation, update resource via API → expect 200 OK, returned DTO reflects updated fields.

- **ChangeStatus_System:**  
  Patch resource status to Inactive → returned DTO shows correct status; patch back to Active → status updated correctly.

- **SearchResources_System:**  
  Query resources via API → returned list matches filter criteria; correct number of resources and fields returned.

**Validation Criteria:**

- End-to-end HTTP flows return expected status codes and payloads.
- Changes are persisted and retrievable.
- Search results match applied filters.

**Expected Outcome:**  
System tests confirm that the creation, update, status change, and search workflows function correctly via the API.

---

## 4.4 Functional Tests

**Test PhysicalResource Lifecycle:**  
User creates a resource, updates it, changes its status, searches resources → all steps validated via API; qualifications and other metadata managed correctly.

**Conclusion:**  
The combination of unit, integration, system, and functional tests provides comprehensive coverage for US2.2.12, ensuring that physical resources are created, updated, managed, and retrieved correctly according to business rules and persist consistently.
