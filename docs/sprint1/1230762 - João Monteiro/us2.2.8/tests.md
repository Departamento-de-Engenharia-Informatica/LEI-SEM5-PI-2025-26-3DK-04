# 4. Unit, Integration, and System Tests

**User Story:** US2.2.8 – Create/Submit Vessel Visit Notification

---

## 4.1 Unit Tests – `VesselVisitNotificationTests.cs`

**Purpose:**  
Ensure the `VesselVisitNotification` aggregate enforces business rules for creation, updating (in-progress completion), submission, and validation of cargo manifests, containers, and crew information.

### Test Descriptions

- **CreateNotification_WithValidData_Succeeds:**  
  Creating a vessel visit notification with valid vessel data, cargo manifests, and optional crew succeeds; aggregate stores all values correctly.

- **CreateNotification_MissingMandatoryFields_Throws:**  
  Missing vessel information or cargo manifests results in `BusinessRuleValidationException`.

- **AddLoadingAndUnloadingCargo_Succeeds:**  
  Adding loading and unloading cargo manifests with valid containers and weights correctly updates cargo data.

- **AddCargoManifest_WithInvalidContainerId_Throws:**  
  If any container identifier fails ISO 6346:2022 validation, `BusinessRuleValidationException` is thrown.

- **AddCrewMembers_WhenRequired_Succeeds:**  
  Adding crew members (name, citizen ID, nationality) when required for security compliance succeeds; data persisted in the aggregate.

- **AddCrewMember_WithInvalidData_Throws:**  
  Missing or malformed crew member data throws `BusinessRuleValidationException`.

- **ChangeStatus_ToInProgress_Succeeds:**  
  Notification can transition to “In Progress” when cargo data is incomplete; status updated correctly.

- **ChangeStatus_ToSubmitted_Succeeds:**  
  Once all mandatory cargo and vessel data are complete, changing status to “Submitted” succeeds.

- **ChangeStatus_ToSubmitted_WithIncompleteData_Throws:**  
  Attempting to submit a notification with missing cargo manifests or invalid container IDs throws `BusinessRuleValidationException`.

### Validation Criteria

- Business rules throw exceptions on invalid cargo, vessel, or crew data.
- ISO 6346:2022 container validation enforced.
- Status transitions follow valid workflow (Draft → InProgress → Submitted).
- Cargo manifests and crew members are stored and retrievable.

**Expected Outcome:**  
All unit tests pass, ensuring the aggregate enforces creation, completion, submission, and validation rules correctly.

---

## 4.2 Integration Tests – `VesselVisitNotificationIntegrationTests.cs`

**Purpose:**  
Verify correct interaction between controller, service, and repository layers for creating, updating, and submitting vessel visit notifications.

### Test Descriptions

- **CreateVesselVisitNotification_Endpoint_ReturnsCreated:**  
  POST `/api/VesselVisitNotifications` with valid data → returns 201 Created, DTO includes vessel, cargo manifests, and crew details.

- **AddCargoManifest_Endpoint_UpdatesCargoData:**  
  PUT `/api/VesselVisitNotifications/{id}/cargo` → returns 200 OK, cargo manifests persisted and returned correctly.

- **AddCrewMembers_Endpoint_UpdatesCrewData:**  
  PUT `/api/VesselVisitNotifications/{id}/crew` → crew list updated successfully; returned DTO matches input.

- **ChangeStatusToInProgress_Endpoint_UpdatesStatus:**  
  PATCH `/api/VesselVisitNotifications/{id}/status` with InProgress → returns 200 OK; DTO Status = InProgress.

- **ChangeStatusToSubmitted_Endpoint_ValidatesAndUpdates:**  
  PATCH `/api/VesselVisitNotifications/{id}/status` with Submitted → validates all cargo and crew data, returns 200 OK if valid or 400 Bad Request if invalid.

### Validation Criteria

- Correct HTTP status codes (201 Created, 200 OK, 400 Bad Request, 404 Not Found).
- All changes persisted and consistent in repository.
- Status transitions only allowed when data is complete.
- Container IDs validated using ISO 6346:2022 rules.

**Expected Outcome:**  
Integration tests confirm correct orchestration of creation, updates, and submissions through the service and controller layers.

---

## 4.3 System Tests – `VesselVisitNotificationSystemTests.cs`

**Purpose:**  
Test end-to-end workflows via the HTTP API, simulating the full lifecycle of a vessel visit notification from creation to submission.

### Test Descriptions

- **CreateNotification_FullWorkflow_System:**  
  Create a vessel, crew members, and cargo manifests; call POST `/api/VesselVisitNotifications` → expect 201 Created, DTO contains vessel and cargo data.

- **AddCargoAndCrew_FullWorkflow_System:**  
  Add cargo manifests and crew → expect 200 OK, returned DTO includes correct cargo and crew data.

- **SetStatusInProgress_ThenSubmit_System:**  
  Patch status to InProgress, then Submitted → expect correct transitions, DTO reflects final Status = Submitted.

- **InvalidSubmission_System:**  
  Attempt submission with missing cargo or invalid container IDs → expect 400 Bad Request, validation message returned.

### Validation Criteria

- End-to-end HTTP flows return expected status codes and payloads.
- Data persisted and retrievable at each step.
- Invalid submissions prevented with correct error responses.

**Expected Outcome:**  
System tests confirm complete notification lifecycle works via API, ensuring validation, persistence, and status transitions behave correctly.

---

## 4.4 Application Tests – `VesselVisitNotificationApplicationTests.cs`

**Purpose:**  
Validate the application/service layer logic for orchestration of cargo, crew, and status management, using in-memory repositories without an HTTP host.

### Test Descriptions

- **AddNotification_Service_Succeeds:**  
  Service correctly creates notification, validating vessel, cargo, and crew.

- **AddCargoManifest_Service_Succeeds:**  
  Adding valid cargo manifests updates aggregate and persists changes.

- **AddCargoManifest_WithInvalidContainer_Service_Throws:**  
  Invalid container ID (non-ISO 6346 compliant) → throws `BusinessRuleValidationException`.

- **ChangeStatusToInProgress_ThenSubmitted_Service_Succeeds:**  
  Service enforces required data before allowing submission; status changes persisted.

### Validation Criteria

- Service correctly enforces domain rules and workflow transitions.
- Unit-of-work commits performed after each valid operation.
- Invalid data triggers domain exceptions, not persistence.

**Expected Outcome:**  
Application tests ensure orchestration and domain enforcement are correct without depending on API layer.

---

## 4.5 Functional Tests

**Test Vessel Visit Notification Lifecycle:**  
User (Shipping Agent Representative) creates a notification, adds cargo and crew, marks it in progress, completes data, and submits it for approval → all steps validated via API or service layer; cargo and crew information validated and persisted.

---

**Conclusion:**  
The combination of unit, integration, system, application, and functional tests provides comprehensive coverage for US2.2.8, ensuring vessel visit notifications are created, updated, validated, and submitted correctly with cargo and crew information, following all business and ISO compliance rules.
