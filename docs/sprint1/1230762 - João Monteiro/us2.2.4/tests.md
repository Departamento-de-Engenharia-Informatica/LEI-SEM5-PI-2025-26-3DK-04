### 4. Unit, Integration, and System Tests

**User Story:** US2.2.4 - *Create/Update/Inactivate/Activate Storage Areas*

---

#### **4.1 Unit Tests – `StorageAreaTests.cs`**

**Purpose:**  
Ensure the `StorageArea` aggregate enforces business rules for creation, updates, and state management, validating capacity, coordinates, type, and dock assignments.

**Test Descriptions:**

* **CreateStorageArea_WithValidParameters_Succeeds:**  
  Creating a storage area with valid code, designation, type, coordinates, max capacity, and optional initial dock assignments succeeds; aggregate stores values correctly.

* **CreateStorageArea_InvalidCoordinates_Throws:**  
  Coordinates must follow valid "latitude, longitude" format; invalid values throw `BusinessRuleValidationException`.

* **CreateStorageArea_InvalidCapacity_Throws:**  
  Max capacity must be ≥1; otherwise, `BusinessRuleValidationException` is thrown.

* **UpdateStorageArea_WithValidParameters_Succeeds:**  
  Updating a storage area with valid new values correctly updates all fields.

* **UpdateStorageArea_WithInvalidCapacityOrCoordinates_Throws:**  
  Updating with invalid max capacity or incorrectly formatted coordinates throws `BusinessRuleValidationException`.

* **AssignDock_ValidDock_Succeeds:**  
  Assigning a valid dock to a storage area updates the dock assignments correctly.

* **AssignDock_InvalidDock_Throws:**  
  Assigning a null or non-existing dock throws `BusinessRuleValidationException`.

* **UnassignDock_ExistingDock_Succeeds:**  
  Removing an assigned dock correctly updates the assignments.

* **MarkAsInactive_PreventsMultipleInactivation:**  
  Calling `MarkAsInactive` on an already inactive storage area throws `BusinessRuleValidationException`.

* **MarkAsActive_PreventsMultipleActivation:**  
  Calling `MarkAsActive` on an already active storage area throws `BusinessRuleValidationException`.

**Validation Criteria:**

* Domain rules throw `BusinessRuleValidationException` on invalid operations.
* Capacity and coordinates are validated.
* Dock assignments are managed correctly.
* Active status changes correctly on inactivation/activation.

**Expected Outcome:**  
All unit tests pass, ensuring the aggregate enforces creation, update, assignment, and activation rules correctly.

---

#### **4.2 Integration Tests – `StorageAreaIntegrationTests.cs`**

**Purpose:**  
Verify correct interaction between controller, service, and repository layers for storage area lifecycle: create, update, assign/unassign dock, inactivate, activate.

**Test Descriptions:**

* **CreateStorageArea_Endpoint_ReturnsCreated:**  
  Send `POST /api/StorageArea` with valid `CreateStorageAreaDto` → expect `201 Created` and returned DTO with correct values.

* **GetStorageAreaById_ReturnsCorrectArea:**  
  After creation, `GET /api/StorageArea/{id}` returns the storage area with all fields matching the created DTO.

* **UpdateStorageArea_Endpoint_ModifiesArea:**  
  `PUT /api/StorageArea/{id}` with valid update DTO → returns `200 OK` and storage area fields updated accordingly.

* **AssignDock_Endpoint_UpdatesAssignments:**  
  `POST /api/StorageArea/{id}/assignDock` → dock assigned; DTO reflects assigned dock.

* **UnassignDock_Endpoint_RemovesAssignment:**  
  `DELETE /api/StorageArea/{id}/unassignDock/{dockId}` → dock unassigned; DTO reflects empty assignments.

* **InactivateStorageArea_Endpoint_SetsInactive:**  
  `PATCH /api/StorageArea/{id}/inactivate` → DTO `Active = false`.

* **ActivateStorageArea_Endpoint_SetsActive:**  
  `PATCH /api/StorageArea/{id}/activate` → DTO `Active = true`.

**Validation Criteria:**

* Correct HTTP status codes: `201 Created`, `200 OK`, `400 Bad Request`, `404 Not Found`.
* Data persistence consistent across creation, update, assignments, and activation status.

**Expected Outcome:**  
Integration tests confirm that API endpoints orchestrate storage area creation, updates, dock assignments, and status changes correctly.

---

#### **4.3 System Tests – `StorageAreaSystemTests.cs`**

**Purpose:**  
Test end-to-end workflows via HTTP API, simulating real user actions for the storage area lifecycle.

**Test Descriptions:**

* **CreateStorageArea_FullWorkflow_System:**  
  Create vessel types, then create a storage area → expect `201 Created`, DTO with correct fields.

* **UpdateStorageArea_FullWorkflow_System:**  
  After creation, update storage area → expect `200 OK`, DTO reflects updated values.

* **AssignAndUnassignDock_System:**  
  Assign dock → DTO contains dock; unassign → DTO reflects empty assignments.

* **InactivateAndActivate_System:**  
  Patch endpoints to change active status → expect correct `Active` field in returned DTO.

**Validation Criteria:**

* End-to-end HTTP flows return expected status codes and payloads.
* Changes are persisted correctly and retrievable.

**Expected Outcome:**  
System tests confirm storage area creation, updates, dock management, and activation workflows function correctly via API.

---

#### **4.4 Application Tests – `StorageAreaApplicationTests.cs`**

**Purpose:**  
Exercise service/application layer using in-memory repositories to validate orchestration, business rules, and unit-of-work commits without running HTTP host.

**Test Descriptions:**

* **CreateStorageArea_ServiceLayer_Succeeds:**  
  Service creates storage area → persisted with correct values.

* **UpdateStorageArea_ServiceLayer_Succeeds:**  
  Updating via service → fields updated; dock assignments managed correctly.

* **AssignDock_ServiceLayer_Succeeds:**  
  Valid dock assignment → persisted correctly.

* **UnassignDock_ServiceLayer_Succeeds:**  
  Dock unassignment → reflected in entity and repository.

* **InactivateAndActivate_ServiceLayer_Succeeds:**  
  Status changes reflected correctly; repeated calls throw `BusinessRuleValidationException`.

**Validation Criteria:**

* Service layer enforces domain rules.
* Unit-of-work commits performed correctly.
* No invalid storage area is persisted.

**Expected Outcome:**  
Application tests ensure service orchestration and domain rules are correctly enforced.

---

#### **4.5 Functional Tests**

* **Test Storage Area Lifecycle:**  
  User creates storage area, updates it, assigns/unassigns docks, inactivates/activates → all steps validated via API or service layer; metadata and assignments managed correctly.

---

**Conclusion:**  
The combination of **unit, integration, system, and application tests** provides comprehensive coverage for US3.1.1, ensuring storage area creation, update, dock assignment, and activation/inactivation behave correctly according to business rules and persist consistently.
