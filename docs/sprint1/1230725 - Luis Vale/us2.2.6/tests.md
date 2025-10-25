
### 4.x Unit, Integration, and System Tests

**User Story:** US2.3.1 - *Register and Manage Representatives*

---

#### **4.1 Unit Tests – `RepresentativeTests.cs`**

**Purpose:**
Ensure that the business rules of the `Representative` entity function correctly, validating creation, assignment, updates, and state transitions.

**Test Descriptions:**

* **WhenCreatingRepresentativeWithValidData_ThenIsCreatedActive:** verifies that a representative is created as *Active* with valid data.
* **WhenMissingRequiredField_ThenThrows:** ensures that exceptions are thrown when required fields are missing.
* **AssignToOrganization_WhenValid_AssignsSuccessfully:** checks correct assignment of a representative to an organization.
* **AssignToOrganization_WhenAlreadyAssignedToDifferentOrg_Throws:** prevents reassignment to another organization.
* **AssignToOrganization_WhenNull_Throws:** validates that an organization ID is mandatory.
* **Update_ChangesProvidedFieldsOnly:** ensures the `Update` method only changes provided fields.
* **Deactivate_And_Activate_WorkCorrectly:** verifies proper state transitions between *Active* and *Inactive*.
* **Deactivate_WhenAlreadyInactive_Throws / Activate_WhenAlreadyActive_Throws:** prevents redundant state changes.
* **WhenRepresentativeHasOrganization_AssignmentIsRemoved / HasNoOrganization_UnassignDoesNothing:** checks correct unassignment behavior.

**Validation Criteria:**

* All domain rules and business exceptions are enforced.
* Methods modify only the intended data.
* States and relationships remain consistent.

**Expected Outcome:**
All tests pass successfully, ensuring the integrity and robustness of the `Representative` entity.

---

#### **4.2 Integration Tests – `RepresentativeIntegrationTests.cs`**

**Purpose:**
Verify correct interaction between layers (application, infrastructure, and API) for creating, retrieving, and validating representative data.

**Test Descriptions:**

* **GetAll_ReturnsInsertedRepresentative:** confirms that inserted representatives are returned by the API.
* **GetById_ReturnsCorrectRepresentative:** ensures that fetching by ID returns the correct representative.
* **AddRepresentative_CreatesSuccessfully:** validates creation of a new representative via `POST /api/Representatives`.
* **AddRepresentative_DuplicateEmail_ReturnsBadRequest:** ensures the system rejects duplicate emails.

**Validation Criteria:**

* HTTP status codes: `200 OK`, `201 Created`, `400 Bad Request`.
* Proper serialization of `RepresentativeDto`.
* Consistent data persistence and retrieval.

**Expected Outcome:**
The API handles CRUD operations correctly, respecting uniqueness and integrity constraints.

---

#### **4.3 System Tests – `RepresentativeSystemTests.cs`**

**Purpose:**
Validate the complete end-to-end workflow of representative management through the HTTP API, including creation, updates, activation, and deactivation.

**Test Descriptions:**

* **Representative_Full_Workflow_via_HttpApi:**

    1. Creates an organization with an initial representative.
    2. Creates a new representative via API.
    3. Retrieves the created representative (`GET /api/Representatives/{id}`).
    4. Updates the representative (`PUT /api/Representatives/{id}/update`).
    5. Deactivates and reactivates the representative (`PUT /deactivate`, `PUT /activate`).
    6. Attempts to create a duplicate representative, expecting *400 Bad Request*.

**Validation Criteria:**

* The HTTP workflow matches expected business behavior.
* Created data is correctly persisted and retrievable.
* Invalid operations are properly rejected according to business rules.

**Expected Outcome:**
The system successfully completes the full representative lifecycle, enforcing integrity and uniqueness constraints throughout all API interactions.


### 4.4. Functional Test

- **Test Create Representative via UI:**
    - Officer opens organization details and selects “Add Representative.”
    - Officer enters valid data and submits.
    - System displays a success message and representative appears in a list.

- **Test Edit Representative via UI:**
    - Officer edits a representative’s email and phone.
    - System confirms update and shows notification sent message.

- **Test Deactivate and Reactivate Representative:**
    - Officer deactivates a representative; a system marks status as “Inactive.”
    - Officer reactivates the same representative; a system updates status to “Active.”

- **Test Duplicate Data Validation:**
    - Officer attempts to register another representative with the same Citizen ID.
    - System rejects operation and displays a duplicate data message.

---

**Conclusion:**
The unit, integration, and system tests ensure full coverage of **US2.3.1—Register and Manage Representatives**, confirming that the domain, application, and API layers work together coherently and reliably.

    - 

