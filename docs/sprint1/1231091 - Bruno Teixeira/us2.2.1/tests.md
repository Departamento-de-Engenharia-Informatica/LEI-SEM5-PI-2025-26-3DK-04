### 4. Unit, Integration and System Tests

**User Story:** US2.2.1 - *Create and Update Vessel Types*

---

#### **4.1 Unit Tests – `VesselTypeApplicationTests.cs`**

**Objective:**
Verify the correct behavior of the `VesselTypeService` and the `VesselType` entity, ensuring that vessel types can be created, updated, searched, and inactivated according to business rules and operational constraints.

**Test Descriptions:**

* **CreateVesselType_CreatesAndReturnsDto:** validates that a new vessel type is successfully created with valid name, description, capacity, and operational constraints (maxRows, maxBays, maxTiers), and that it is initialized as "Active".
* **CreateVesselType_EmptyName_ThrowsBusinessRule:** ensures that a `BusinessRuleValidationException` is thrown when attempting to create a vessel type with an empty name.
* **CreateVesselType_EmptyDescription_ThrowsBusinessRule:** verifies that a vessel type cannot be created with an empty description.
* **CreateVesselType_InvalidCapacity_ThrowsBusinessRule:** confirms that a vessel type with zero or negative capacity is rejected.
* **CreateVesselType_InvalidMaxRows_ThrowsBusinessRule:** ensures that a vessel type with zero or negative maxRows cannot be created.
* **UpdateVesselType_ChangesAllFields:** validates that all vessel type fields (name, description, capacity, maxRows, maxBays, maxTiers) can be successfully updated and persisted.
* **UpdateVesselType_NonExistentId_ReturnsNull:** verifies that attempting to update a non-existent vessel type returns null without committing changes.
* **UpdateVesselType_InvalidName_ThrowsBusinessRule:** ensures that updating a vessel type with an empty name throws a `BusinessRuleValidationException`.
* **SearchByName_ReturnsMatches:** confirms that vessel types can be searched by name (case-insensitive) and only matching results are returned.
* **SearchByDescription_ReturnsMatches:** validates that vessel types can be searched by description keywords.
* **SearchByNameOrDescription_ReturnsMatches:** verifies that the general search returns vessel types matching either name or description.
* **GetAllVesselTypes_ReturnsAll:** ensures that all vessel types are correctly retrieved.
* **InactivateVesselType_MarksAsInactive:** validates that a vessel type can be inactivated and the change is persisted.

**Validation Criteria:**

* Business rules are respected.
* Exceptions of type `BusinessRuleValidationException` are thrown as expected.
* Data is correctly persisted and retrieved from the repository.
* UnitOfWork commits transactions appropriately.

**Expected Result:**
All tests pass successfully, ensuring correct behavior of vessel type creation, updates, search functionality, and inactivation.

---

#### **4.2 Integration Tests – `VesselTypeIntegrationTests.cs`**

**Objective:**
Validate communication between the application layer, API controller, and database (`DDDSample1DbContext`), ensuring correct persistence and retrieval of vessel types through the REST API.

**Test Descriptions:**

* **GetAll_ReturnsInsertedVesselType:** confirms that a previously inserted vessel type is returned by the API via `GET /api/VesselTypes`.
* **GetById_ReturnsCorrectVesselType:** verifies that fetching by ID returns the correct vessel type with all attributes.
* **Create_CreatesNewVesselType:** validates the full process of creating a new vessel type via `POST /api/VesselTypes`.
* **Create_WithInvalidData_ReturnsBadRequest:** ensures that attempting to create a vessel type with invalid data (empty name, invalid capacity) returns *400 Bad Request*.
* **Update_ModifiesVesselType:** confirms that updating a vessel type via `PUT /api/VesselTypes/{id}` correctly modifies and persists the data.
* **SearchByName_ReturnsFilteredResults:** validates that the search endpoint correctly filters vessel types by name.
* **Inactivate_MarksAsInactive:** verifies that the inactivation endpoint correctly changes the active status.

**Validation Criteria:**

* Correct HTTP response codes (`200`, `201`, `400`, `404`).
* JSON content matches the `VesselTypeDto` model.
* Data is properly persisted in the test database.
* Search and filter operations work correctly.

**Expected Result:**
All API endpoints behave as expected, respecting validation and data integrity rules.

---

#### **4.3 System Tests – `VesselTypeSystemTests.cs`**

**Objective:**
Validate the complete workflow of managing vessel types through the HTTP API, simulating real user behavior and full operation lifecycle.

**Test Descriptions:**

* **VesselType_Full_Workflow_via_HttpApi:**

    1. Creates a vessel type via `POST /api/VesselTypes`.
    2. Retrieves the created vessel type via `GET /api/VesselTypes/{id}`.
    3. Updates the vessel type via `PUT /api/VesselTypes/{id}`.
    4. Searches for vessel types by name via `GET /api/VesselTypes/search`.
    5. Fetches all vessel types via `GET /api/VesselTypes`.
    6. Inactivates the vessel type via `PATCH /api/VesselTypes/{id}/inactivate`.
    7. Verifies that the vessel type is marked as inactive.

**Validation Criteria:**

* The system maintains consistency between HTTP calls.
* Created and updated data is retrievable and visible in subsequent queries.
* Responses and status codes correctly reflect business rules.
* Inactivated vessel types cannot be used for new vessel registrations.

**Expected Result:**
The complete workflow executes successfully, demonstrating that the API correctly supports creation, retrieval, update, search, and inactivation of vessel types.

### 4.4. Functional Tests

- **Test Vessel Type Creation via UI:**
    - Port Authority Officer logs in and navigates to "Create Vessel Type".
    - Officer fills in all required fields (name, description, capacity, maxRows, maxBays, maxTiers).
    - System validates inputs and confirms creation.
    - Verify the new vessel type appears in the vessel types list.

- **Test Error on Invalid Input:**
    - Officer attempts to create a vessel type with empty name or invalid capacity.
    - System displays validation errors and prevents submission.

- **Test Vessel Type Update:**
    - Officer navigates to an existing vessel type's edit page.
    - Officer modifies fields (description, capacity, operational constraints).
    - System confirms update and displays the modified data.

- **Test Search and Filter:**
    - Officer searches vessel types by name or description.
    - System displays only matching results.
    - Verify partial matches work correctly.

- **Test Inactivate Vessel Type:**
    - Officer selects an active vessel type and clicks "Inactivate".
    - System marks it as inactive.
    - Verify it's no longer available for new vessel registrations.

---

**Conclusion:**
The tests performed across all three layers (unit, integration, and system) validate the full behavior of *User Story US2.2.1*, ensuring that vessel types can be created, updated, searched, and inactivated according to all defined business rules, integrations, and operational flows.
