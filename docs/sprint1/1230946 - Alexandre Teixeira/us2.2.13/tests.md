## 4. Tests

**User Story:** US2.2.13 - Qualifications (create, update, search, delete)

---

#### **4.1 Unit Tests – `QualificationTests.cs`**

**Purpose:**
Validate domain invariants for `Qualification` (constructor validation, name rules, updates and identity).

**Test Descriptions:**

* **WhenPassingCorrectData_ThenQualificationIsInstantiated:** valid names create an entity with a non-null Id.

* **WhenPassingInvalidData_ThenBusinessRuleValidationExceptionIsThrown:** names that don't follow rules (single word, invalid chars) throw domain validation.

* **WhenPassingEmptyOrNullName_ThenThrowsException:** empty/null names produce the expected validation message.

* **WhenPassingNameWithLessThanTwoWords_ThenThrowsException:** qualification name must contain at least two words.

* **WhenPassingNameWithMoreThan150Characters_ThenThrowsException:** maximum length validation enforced on creation and change.

* **WhenChangingToValidName_ThenNameIsUpdated / WhenChangingToInvalidName_ThenThrows:** ChangeName enforces same rules as constructor.

* **WhenCreatingTwoQualifications_ShouldHaveDifferentIds:** identity uniqueness.

**Validation Criteria:**

* Domain rules throw `BusinessRuleValidationException` for invalid names and updates.

**Expected Outcome:**
Unit tests ensure `Qualification` domain enforces naming and identity invariants.

---

#### **4.2 Integration Tests – `QualificationIntegrationTests.cs`**

**Purpose:**
Verify controller ↔ service ↔ repository interactions for Qualifications endpoints.

**Test Descriptions:**

* **GetAll_ReturnsInsertedQualification:** seed a Qualification in DB and GET `/api/Qualifications` returns it.

* **GetById_ReturnsCorrectQualification:** seed and GET by id returns correct DTO.

* **Create_AddsQualification:** POST `/api/Qualifications` returns 201 Created and the created DTO.

* **Update_ChangesQualification:** PUT `/api/Qualifications/{id}` updates and persists the name.

* **Delete_RemovesQualification:** DELETE removes the entity and subsequent GET returns NotFound.

* **SearchByName_ReturnsMatches:** seed multiple qualifications and search endpoint filters by name.

* **ExistsById_ReturnsTrueOrFalse:** GET `/api/Qualifications/exists/{id}` returns a boolean wrapper indicating existence.

**Validation Criteria:**

* Correct status codes (`200`, `201`, `204`, `404`).
* Response DTOs reflect persisted data.

**Expected Outcome:**
Integration tests validate the HTTP API, persistence and search/existence behaviours.

---

#### **4.3 System Tests – `QualificationSystemTests.cs`**

**Purpose:**
End-to-end verification via the test host for Qualifications CRUD and listing workflows.

**Test Descriptions:**

* **Qualification_CRUD_Workflow_via_HttpApi:** create via POST, GET by id, update via PUT, GET all, DELETE and verify NotFound — all using real HTTP calls to the test server.

**Validation Criteria:**

* End-to-end HTTP flows return expected status codes and payloads.

**Expected Outcome:**
System tests provide high-confidence coverage of the Qualifications API surface.

---

#### **4.4 Application Tests – `QualificationApplicationTests.cs`**

**Purpose:**
Exercise service layer with in-memory repositories and a fake unit-of-work to validate creation, search, update and deletion logic and commit behaviour.

**Test Descriptions:**

* **CreateQualification_CreatesAndReturnsDto:** service creates entity, commits, and returns DTO.

* **SearchByName_ReturnsMatches:** service search works against in-memory data.

* **CreateQualification_InvalidName_ThrowsBusinessRule:** service rejects invalid names and avoids committing.

* **UpdateQualification_ChangesName:** service updates name and commits.

* **DeleteQualification_RemovesEntity:** service deletes entity and commits.

**Validation Criteria:**

* Service produces DTOs with expected state and in-memory UoW commit counts reflect operations.

**Expected Outcome:**
Application tests validate service logic, error handling and unit-of-work behaviour quickly and deterministically.

---

#### **4.5 Functional Tests**

* **Qualification lifecycle:** create → update → list/search → delete (user-facing flows).

---

**Conclusion:**
Combined unit, integration, application and system tests ensure Qualifications domain correctness, service orchestration and end-to-end API behaviour for US2.2.13.


