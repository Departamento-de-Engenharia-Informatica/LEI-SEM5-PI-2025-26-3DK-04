### 4. Unit, Integration, and System Tests

**User Story:** US2.2.3 - *Create/Update Docks*

---

#### **4.1 Unit Tests – `DockTests.cs`**

**Purpose:**  
Ensure that the `Dock` aggregate enforces business rules for creation, updates, and state management, validating dimensions, allowed vessel types, and required fields.

**Test Descriptions:**

* **CreateDock_WithValidParameters_Succeeds:**  
  Creating a dock with valid name, length, depth, max draft, location, and at least one vessel type succeeds; aggregate stores values correctly.

* **CreateDock_WithoutName_Throws:**  
  Name is required; if null or whitespace, `BusinessRuleValidationException` is thrown.

* **CreateDock_WithNoVesselTypes_Throws:**  
  A dock must have at least one allowed vessel type; otherwise, `BusinessRuleValidationException` is thrown.

* **UpdateDock_WithValidParameters_Succeeds:**  
  Updating a dock with valid new values correctly updates all fields including allowed vessel types.

* **UpdateDock_WithInvalidDimensions_Throws:**  
  Updating with length, depth, or max draft ≤ 0 throws `BusinessRuleValidationException`.

* **MarkAsInactive_PreventsMultipleInactivation:**  
  Calling `MarkAsInactive` on an already inactive dock throws `BusinessRuleValidationException`.

**Validation Criteria:**

* Domain rules throw `BusinessRuleValidationException` on invalid operations.
* Allowed vessel types collection is always non-empty after creation/update.
* Dock status (`Active`) changes correctly on `MarkAsInactive`.

**Expected Outcome:**  
All unit tests pass, ensuring the aggregate enforces creation and update rules correctly.

---

#### **4.2 Integration Tests – `DockIntegrationTests.cs`**

**Purpose:**  
Verify correct interaction between controller, service, and repository layers for creating, updating, and inactivating docks.

**Test Descriptions:**

* **CreateDock_Endpoint_ReturnsCreated:**  
  Send `POST /api/Dock` with valid `DockDto` → expect `201 Created` and returned DTO with correct values and `Id`.

* **GetDockById_ReturnsCorrectDock:**  
  After creation, `GET /api/Dock/{id}` returns the dock with all fields matching the created DTO.

* **UpdateDock_Endpoint_ModifiesDock:**  
  `PUT /api/Dock/{id}` with valid update DTO → returns `200 OK` and dock fields updated accordingly.

* **UpdateDock_WithInvalidData_ReturnsBadRequest:**  
  Attempting to update dock with invalid length/depth/max draft → returns `400 Bad Request`.

* **SoftDeleteDock_MarksAsInactive:**  
  `DELETE /api/Dock/{id}` → returns `200 OK` and dock is marked inactive; further `DELETE` → returns `400 Bad Request`.

* **HardDeleteDock_RemovesDock:**  
  `DELETE /api/Dock/{id}/hard` → removes dock; subsequent `GET /api/Dock/{id}` → `404 Not Found`.

**Validation Criteria:**

* Correct HTTP status codes: `201 Created`, `200 OK`, `400 Bad Request`, `404 Not Found`.
* Data persistence consistent: creation, update, soft delete, and hard delete reflected in database.

**Expected Outcome:**  
Integration tests confirm that the API endpoints properly orchestrate creation, updates, and deletion of docks.

---

#### **4.3 System Tests – `DockSystemTests.cs`**

**Purpose:**  
Test end-to-end workflows via HTTP API, simulating real user actions for the dock lifecycle.

**Test Descriptions:**

* **CreateDock_FullWorkflow_System:**  
  End-to-end: create vessel types via API, then create a dock → expect `201 Created`, returned DTO with correct values.

* **UpdateDock_FullWorkflow_System:**  
  After creation, update dock via API → expect `200 OK`, returned DTO reflects updated fields.

* **SoftDeleteDock_System:**  
  Mark dock as inactive via API → dock returned with `Active = false`; subsequent soft delete → `400 Bad Request`.

* **HardDeleteDock_System:**  
  Hard delete a dock via API → returns `200 OK`; subsequent `GET` → `404 Not Found`.

**Validation Criteria:**

* End-to-end HTTP flows return expected status codes and payloads.
* Changes are persisted correctly and retrievable.

**Expected Outcome:**  
System tests confirm that the dock creation, update, and deletion workflows function correctly via the API.

---

#### **4.4 Application Tests – `DockApplicationTests.cs`**

**Purpose:**  
Exercise service/application layer using in-memory repositories to validate orchestration, business rules, and unit-of-work commits without running HTTP host.

**Test Descriptions:**

* **CreateDock_ServiceLayer_Succeeds:**  
  Service creates dock with valid parameters → dock persisted with correct values.

* **UpdateDock_ServiceLayer_Succeeds:**  
  Updating dock via service → fields updated; allowed vessel types replaced correctly.

* **UpdateDock_WithInvalidParameters_Throws:**  
  Service throws `BusinessRuleValidationException` if invalid length/depth/max draft provided.

* **MarkDockAsInactive_ServiceLayer_Succeeds:**  
  Service inactivates dock → `Active` set to `false`.

* **DuplicateInactivation_PreventsMultipleCalls:**  
  Second call to inactivate an already inactive dock → throws `BusinessRuleValidationException`.

**Validation Criteria:**

* Service layer enforces domain rules.
* Unit-of-work commits performed correctly.
* No invalid dock is persisted.

**Expected Outcome:**  
Application tests ensure that service orchestration and domain rules are correctly enforced.

---

#### **4.5 Functional Tests**

* **Test Dock Lifecycle:**  
  User creates dock, updates it, marks it inactive, optionally hard deletes → all steps validated via API or service layer; metadata and allowed vessel types managed correctly.

---

**Conclusion:**  
The combination of **unit, integration, system, and application tests** provides comprehensive coverage for US2.2.3, ensuring dock creation, update, and deletion behave correctly according to business rules and persist consistently.
