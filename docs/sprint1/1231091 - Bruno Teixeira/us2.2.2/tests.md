### 4. Unit, Integration and System Tests

**User Story:** US2.2.2 - *Register and Update Vessel Records*

---

#### **4.1 Unit Tests – `VesselApplicationTests.cs`**

**Objective:**
Verify the correct behavior of the `VesselService` and the `Vessel` entity, ensuring that vessels can be registered, updated, and searched while maintaining data integrity and IMO number uniqueness.

**Test Descriptions:**

* **CreateVessel_CreatesAndReturnsDto:** validates that a new vessel is successfully created with valid IMO number, name, vessel type reference, owner, and operator, and that it is initialized as "Active".
* **CreateVessel_EmptyImoNumber_ThrowsBusinessRule:** ensures that a `BusinessRuleValidationException` is thrown when attempting to create a vessel with an empty IMO number.
* **CreateVessel_InvalidImoFormat_ThrowsBusinessRule:** verifies that vessels with IMO numbers not following the official format (IMO + 7 digits) are rejected.
* **CreateVessel_DuplicateImoNumber_ThrowsBusinessRule:** confirms that creating a vessel with an already existing IMO number throws an exception, maintaining uniqueness.
* **CreateVessel_NonExistentVesselType_ThrowsBusinessRule:** ensures that a vessel cannot be created with a non-existent vessel type reference.
* **CreateVessel_EmptyName_ThrowsBusinessRule:** verifies that a vessel cannot be created with an empty name.
* **UpdateVessel_ChangesAllFields:** validates that vessel fields (name, vessel type, owner, operator) can be successfully updated while the IMO number remains immutable.
* **UpdateVessel_NonExistentId_ReturnsNull:** verifies that attempting to update a non-existent vessel returns null without committing changes.
* **UpdateVessel_InvalidName_ThrowsBusinessRule:** ensures that updating a vessel with an empty name throws a `BusinessRuleValidationException`.
* **SearchByName_ReturnsMatches:** confirms that vessels can be searched by name (case-insensitive) with partial matching support.
* **SearchByOwner_ReturnsMatches:** validates that vessels can be filtered by owner name.
* **SearchByOperator_ReturnsMatches:** verifies that vessels can be filtered by operator name.
* **GetByImoNumber_ReturnsVessel:** ensures that a vessel can be retrieved by its unique IMO number.
* **GetAllVessels_ReturnsAll:** validates that all vessels are correctly retrieved.
* **InactivateVessel_MarksAsInactive:** confirms that a vessel can be inactivated and the change is persisted.

**Validation Criteria:**

* Business rules are respected, especially IMO number format and uniqueness.
* Exceptions of type `BusinessRuleValidationException` are thrown as expected.
* Vessel type references are validated.
* Data is correctly persisted and retrieved from the repository.
* UnitOfWork commits transactions appropriately.

**Expected Result:**
All tests pass successfully, ensuring correct behavior of vessel registration, updates, search functionality, and inactivation while maintaining referential integrity with vessel types.

---

#### **4.2 Integration Tests – `VesselIntegrationTests.cs`**

**Objective:**
Validate communication between the application layer, API controller, and database (`DDDSample1DbContext`), ensuring correct persistence and retrieval of vessels through the REST API, including validation of vessel type relationships.

**Test Descriptions:**

* **GetAll_ReturnsInsertedVessel:** confirms that a previously inserted vessel is returned by the API via `GET /api/Vessels`.
* **GetById_ReturnsCorrectVessel:** verifies that fetching by ID returns the correct vessel with all attributes including vessel type information.
* **Create_CreatesNewVessel:** validates the full process of creating a new vessel via `POST /api/Vessels`.
* **Create_WithInvalidImo_ReturnsBadRequest:** ensures that attempting to create a vessel with an invalid IMO format returns *400 Bad Request*.
* **Create_WithDuplicateImo_ReturnsBadRequest:** verifies that duplicate IMO numbers are rejected with *400 Bad Request*.
* **Create_WithNonExistentVesselType_ReturnsBadRequest:** confirms that referencing a non-existent vessel type returns *400 Bad Request*.
* **Update_ModifiesVessel:** validates that updating a vessel via `PUT /api/Vessels/{id}` correctly modifies the data while keeping IMO immutable.
* **SearchByImoNumber_ReturnsCorrectVessel:** confirms that the search endpoint can find vessels by their unique IMO number.
* **SearchByName_ReturnsFilteredResults:** validates that the search endpoint correctly filters vessels by name.
* **Inactivate_MarksAsInactive:** verifies that the inactivation endpoint correctly changes the active status.

**Validation Criteria:**

* Correct HTTP response codes (`200`, `201`, `400`, `404`).
* JSON content matches the `VesselDto` model.
* Data is properly persisted in the test database.
* Vessel type relationships are maintained.
* IMO uniqueness is enforced.
* Search and filter operations work correctly.

**Expected Result:**
All API endpoints behave as expected, respecting validation, data integrity rules, and referential integrity with vessel types.

---

#### **4.3 System Tests – `VesselSystemTests.cs`**

**Objective:**
Validate the complete workflow of managing vessels through the HTTP API, simulating real user behavior and full operation lifecycle, including vessel type dependency.

**Test Descriptions:**

* **Vessel_Full_Workflow_via_HttpApi:**

    1. Creates a vessel type via `POST /api/VesselTypes` (prerequisite).
    2. Creates a vessel via `POST /api/Vessels` referencing the vessel type.
    3. Retrieves the created vessel via `GET /api/Vessels/{id}`.
    4. Verifies vessel type information is included in the response.
    5. Updates the vessel via `PUT /api/Vessels/{id}` (changing owner/operator).
    6. Confirms IMO number remains unchanged after update.
    7. Searches for vessels by name via `GET /api/Vessels/search`.
    8. Searches for vessels by IMO number via `GET /api/Vessels/imo/{imoNumber}`.
    9. Fetches all vessels via `GET /api/Vessels`.
    10. Attempts to create a vessel with duplicate IMO, expecting *400 Bad Request*.
    11. Inactivates the vessel via `PATCH /api/Vessels/{id}/inactivate`.
    12. Verifies that the vessel is marked as inactive.

**Validation Criteria:**

* The system maintains consistency between HTTP calls.
* Created and updated data is retrievable and visible in subsequent queries.
* Responses and status codes correctly reflect business rules.
* Vessel type relationships are properly maintained.
* IMO number uniqueness is enforced across the system.
* IMO number immutability is preserved during updates.
* Inactivated vessels cannot be used for new visit notifications.

**Expected Result:**
The complete workflow executes successfully, demonstrating that the API correctly supports vessel registration, retrieval, update, search, and inactivation while maintaining referential integrity with vessel types and enforcing IMO number constraints.

### 4.4. Functional Tests

- **Test Vessel Registration via UI:**
    - Port Authority Officer logs in and navigates to "Register Vessel".
    - Officer fills in all required fields (IMO number, name, vessel type, owner, operator).
    - System validates IMO format and checks for duplicates.
    - System confirms creation and displays success message.
    - Verify the new vessel appears in the vessels list with vessel type information.

- **Test Error on Invalid IMO:**
    - Officer attempts to register a vessel with invalid IMO format.
    - System displays validation error explaining IMO format requirements.
    - Prevents submission until valid IMO is provided.

- **Test Duplicate IMO Prevention:**
    - Officer tries to register a vessel with an existing IMO number.
    - System blocks submission and displays error message.
    - Suggests checking if the vessel already exists.

- **Test Vessel Type Selection:**
    - Officer selects vessel type from dropdown during vessel registration.
    - System displays only active vessel types.
    - Validates that selected vessel type exists.

- **Test Vessel Update:**
    - Officer navigates to an existing vessel's edit page.
    - Officer modifies owner, operator, or vessel type.
    - IMO number field is disabled/read-only.
    - System confirms update and displays the modified data.

- **Test Search by IMO:**
    - Officer enters IMO number in search field.
    - System quickly locates and displays the matching vessel.

- **Test Search and Filter:**
    - Officer filters vessels by name, owner, or operator.
    - System displays only matching results.
    - Verify partial matches work correctly.

- **Test Inactivate Vessel:**
    - Officer selects an active vessel and clicks "Inactivate".
    - System marks it as inactive.
    - Verify it's no longer available for new visit notifications.

---

**Conclusion:**
The tests performed across all three layers (unit, integration, and system) validate the full behavior of *User Story US2.2.2*, ensuring that vessels can be registered and updated according to all defined business rules, with proper IMO number validation and uniqueness enforcement, while maintaining referential integrity with vessel types.
