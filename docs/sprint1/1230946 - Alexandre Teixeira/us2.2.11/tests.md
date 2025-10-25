## 4. Tests

**User Story:** US2.2.11 - Staff Member management (creation, update, qualifications, activation)

---

#### **4.1 Unit Tests – `StaffMemberTests.cs`**

**Purpose:**
Validate domain invariants and business rules for `StaffMember` (construction, updates, qualifications and status transitions).

**Test Descriptions:**

* **WhenPassingCorrectData_ThenStaffMemberIsInstantiated:** verifies valid constructor inputs create a StaffMember with default Available status and non-null Id.

* **WhenPassingInvalidEmail_ThenBusinessRuleValidationExceptionIsThrown:** invalid emails (empty, missing @) throw domain validation.

* **WhenPassingInvalidPhoneNumber_ThenBusinessRuleValidationExceptionIsThrown:** invalid phone numbers throw domain validation.

* **WhenChangingToValidName_ThenNameIsUpdated / WhenChangingToInvalidName_ThenThrows:** name updates validate non-empty values.

* **WhenChangingToValidEmail_ThenEmailIsUpdated / WhenChangingToInvalidEmail_ThenThrows:** email change validation.

* **WhenChangingPhoneNumberToValid_ThenUpdated / WhenChangingPhoneNumberToInvalid_ThenThrows:** phone number validation on change.

* **WhenAddingQualification_ThenItIsAdded / WhenAddingNullQualification_ThenThrows / WhenAddingDuplicateQualification_ThenThrows:** qualification add/remove behaviours and duplicates are prevented.

* **WhenDeactivatingAndReactivating_StatusChangesAppropriately:** deactivation/reactivation transitions and idempotency checks (double deactivate/reactivate raise errors).

* **WhenCreatingTwoStaffMembers_ShouldHaveDifferentIds:** identity uniqueness.

**Validation Criteria:**

* Domain rules throw `BusinessRuleValidationException` for invalid operations.
* State transitions result in the expected `MemberStatus`.

**Expected Outcome:**
All unit tests pass, guaranteeing domain invariants for `StaffMember`.

---

#### **4.2 Integration Tests – `StaffMemberIntegrationTests.cs`**

**Purpose:**
Check controller ↔ service ↔ repository interactions for StaffMember endpoints and qualification wiring.

**Test Descriptions:**

* **GetAll_ReturnsInsertedStaffMember:** seed a StaffMember in the test DB and GET `/api/StaffMembers` returns it.

* **GetById_ReturnsCorrectStaffMember:** seed and GET by id returns correct DTO.

* **Create_AddsStaffMember:** POST `/api/StaffMembers` returns 201 Created and correct DTO.

* **Update_ChangesStaffMember:** PUT `/api/StaffMembers/{id}` updates fields and persists.

* **Deactivate_And_Reactivate_StaffMember:** DELETE then PUT `/reactivate` transitions status and affects listing.

* **AddAndRemoveQualification_Works:** seed qualification and staff, POST to add and DELETE to remove qualification via API.

* **SearchByName_ReturnsMatches:** seed multiple staff and call search endpoint to validate filtering.

**Validation Criteria:**

* Correct HTTP status codes (`200 OK`, `201 Created`).
* DTOs returned match persisted data and lists reflect activation status.

**Expected Outcome:**
Integration tests validate HTTP layer, service orchestration and persistence behavior for StaffMember workflows.

---

#### **4.3 System Tests – `StaffMemberSystemTests.cs`**

**Purpose:**
End-to-end verification using the running test host and real HTTP calls to exercise the full application stack for StaffMember CRUD and qualification flows.

**Test Descriptions:**

* **StaffMember_CRUD_and_Qualifications_Workflow_via_HttpApi:** create staff via API, retrieve, update, list, create qualification via API, add/remove qualification, deactivate/reactivate and verify lists — all via HTTP calls against the test server.

**Validation Criteria:**

* End-to-end HTTP flows return expected status codes and payloads.
* Persistence layer reflects the changes and endpoints return consistent DTOs.

**Expected Outcome:**
System tests provide high-confidence coverage for StaffMember user flows under a near-production API surface.

---

#### **4.4 Application Tests – `StaffMemberApplicationTests.cs`**

**Purpose:**
Exercise the application/service layer using in-memory repositories and a test unit-of-work to validate service orchestration, commit counts and repository interactions without starting the HTTP host.

**Test Descriptions:**

* **CreateStaffMember_CreatesAndReturnsDto:** service creates entity, commits and returns DTO.

* **SearchByName_ReturnsMatches:** service search behaviour with in-memory data.

* **UpdateStaffMember_ChangesFields:** service updates fields and commits once.

* **DeactivateAndReactivate_Works:** service-level status transitions and uow commits.

* **AddAndRemoveQualification_Works:** service correctly wires qualifications and commits expected times.

**Validation Criteria:**

* Service returns DTOs with updated state and repositories record expected changes.
* In-memory unit-of-work commit counts verify transactional behaviour.

**Expected Outcome:**
Application tests validate service logic and integration with repository/UoW abstractions quickly and deterministically.

---

#### **4.5 Functional Tests**

* **Staff member lifecycle:** creation → update → qualification management → deactivate → reactivate (exercise expected user-facing scenarios).

---

**Conclusion:**
Unit, integration, application and system tests together ensure the correctness of StaffMember domain rules, service orchestration and end-to-end HTTP behaviour for US2.2.11.


