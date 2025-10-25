### 4. Unit, Integration and System Tests

**User Story:** US2.2.5 - *Register New Shipping Agent Organizations*

---

#### **4.1 Unit Tests – `OrganizationTests.cs`**

**Objective:**
Verify the correct behavior of the domain rules of the `Organization` entity and its aggregation with `Representative`, ensuring business integrity and required validations.

**Test Descriptions:**

* **WhenCreatingWithValidData_ThenIsCreated:** validates that an organization is correctly created with valid data.
* **WhenMissingRequiredField_ThenThrows:** ensures that exceptions are thrown when mandatory fields (ID, LegalName, Address) are missing.
* **AddRepresentative_AssignsSuccessfully:** confirms that a representative is successfully added to the organization.
* **AddRepresentative_WhenNull_Throws:** verifies that a null representative cannot be added.
* **AddRepresentative_WhenSameEmailOrPhone_Throws:** ensures that adding representatives with duplicate email or phone numbers in the same organization is not allowed.
* **AddRepresentative_WhenBelongsToAnotherOrg_Throws:** prevents associating a representative already assigned to another organization.
* **HasRepresentative_ReturnsTrueWhenListNotEmpty:** checks if the method correctly identifies the existence of representatives.
* **ValidateReadyForRegistration_WithoutReps_Throws:** guarantees that an organization without representatives cannot be registered.
* **RemoveRepresentative_RemovesSuccessfully / WhenNull_DoesNothing:** validates successful removal and proper handling of null values.

**Validation Criteria:**

* Business rules are respected.
* Exceptions of type `BusinessRuleValidationException` are thrown as expected.
* The integrity of the representative list is maintained.

**Expected Result:**
All tests pass successfully, ensuring correct domain layer behavior.

---

#### **4.2 Integration Tests – `OrganizationIntegrationTests.cs`**

**Objective:**
Validate communication between the application layer, API controller, and database (`DDDSample1DbContext`), ensuring correct persistence and retrieval of organizations.

**Test Descriptions:**

* **GetAll_ReturnsInsertedOrganization:** confirms that a previously inserted organization is returned by the API.
* **GetById_ReturnsCorrectOrganization:** verifies that fetching by ID returns the correct organization.
* **Register_CreatesNewOrganization:** validates the full process of creating a new organization via `POST /api/Organizations`.
* **Register_WithoutRepresentatives_ReturnsBadRequest:** ensures that trying to register an organization without representatives returns *400 Bad Request*.
* **Register_WithDuplicateId_ReturnsBadRequest:** verifies that duplicate IDs are rejected.

**Validation Criteria:**

* Correct HTTP response codes (`200`, `201`, `400`).
* JSON content matches the `OrganizationDto` model.
* Data is properly persisted in the test database.

**Expected Result:**
All API endpoints behave as expected, respecting validation and data integrity rules.

---

#### **4.3 System Tests – `OrganizationSystemTests.cs`**

**Objective:**
Validate the complete workflow of registering an organization through the HTTP API, simulating real user behavior and full operation lifecycle.

**Test Descriptions:**

* **Organization_Full_Workflow_via_HttpApi:**

    1. Creates an organization via `POST /api/Organizations`.
    2. Retrieves the created organization via `GET /api/Organizations/{id}`.
    3. Fetches all organizations via `GET /api/Organizations`.
    4. Attempts to create the same organization again, expecting *400 Bad Request*.

**Validation Criteria:**

* The system maintains consistency between HTTP calls.
* Created data is retrievable and visible in subsequent queries.
* Responses and status codes correctly reflect business rules.

**Expected Result:**
The complete workflow executes successfully, demonstrating that the API correctly supports registration, persistence, and retrieval of new organizations, while preventing invalid duplicates.

### 4.4. Functional Tests
- **Test Organization Registration via UI:**
    - Officer logs in and navigates to "Register New Organization".
    - Officer fills in organization details and adds at least one representative.
    - System validates inputs and confirms creation.
    - Verify a new organization appears in an organization list with representative linked.

- **Test Error on Missing Fields:**
    - Officer attempts to register an organization with a missing tax number or address.
    - System displays validation errors and prevents submission.

- **Test Duplicate Organization Prevention:**
    - Officer tries to create an organization with existing identifier.
    - System blocks and displays an error message.

---

**Conclusion:**
The tests performed across all three layers (unit, integration, and system) validate the full behavior of *User Story US2.2.5*, ensuring that the registration of new *Shipping Agent Organizations* complies with all defined business rules, integrations, and operational flows.



