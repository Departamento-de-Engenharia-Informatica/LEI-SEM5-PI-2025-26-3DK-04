### 4. Unit, Integration, and System Tests

**User Story:** US2.2.7 - *Review Pending Vessel Visit Notifications*

---

#### **4.1 Unit Tests – `VesselVisitNotificationTests.cs`**

**Purpose:**  
Ensure that the business rules of the `VesselVisitNotification` aggregate function correctly, validating updates, submission, withdrawal, resumption, approval, rejection and state constraints.

**Test Descriptions:**

* **Approve_And_Reject_RequireCompletedState_AndValidateParameters:** approves/rejects only from `Completed`; validates required parameters (dock/officer or reason/officer) and records decision metadata.
 
* **Approve_FromCompleted_Succeeds:** when a notification is set to `Completed`, calling `Approve(...)` sets status to `Approved`, stores `AssignedDock`, `OfficerId`, `DecisionOutcome` and a timestamp.

* **Reject_FromCompleted_Succeeds:** when a notification is set to `Completed`, calling `Reject(...)` sets status to `Rejected`, stores `RejectedReason`, `OfficerId`, `DecisionOutcome` and a timestamp.

* **Approve_WithoutDock_Throws:** approving a `Completed` notification without specifying a dock throws `BusinessRuleValidationException` with message "A dock must be assigned when approving a notification.".

* **Reject_WithoutReason_Throws:** rejecting a `Completed` notification without a reason throws `BusinessRuleValidationException` with message "A rejection reason must be provided.".

* **DuplicateDecision_AfterApproved_Throws:** after a notification has been approved, further attempts to approve or reject are rejected by business rules (throws `BusinessRuleValidationException`).

**Validation Criteria:**

* Domain rules throw `BusinessRuleValidationException` on invalid operations.

**Expected Outcome:**  
All unit tests pass, ensuring the aggregate enforces the lifecycle and validations.

---

#### **4.2 Integration Tests – `VesselVisitNotificationIntegrationTests.cs`**

**Purpose:**  
Verify correct interaction between layers (controller, service, repository) when updating, submitting, withdrawing, resuming, approving and rejecting notifications.

**Test Descriptions:**

* **RejectAfterCompleted_AllowsRejection:** Seed a vessel type, vessel, organization and representative; create a notification, set its `Status` to `Completed` directly in the test DbContext, call the controller `PUT /api/VesselVisitNotifications/{id}/reject` with `{ Reason, OfficerId }`. Expect `200 OK`, returned `status` = `Rejected` and `rejectedReason` set to the provided value.

* **RejectWithoutReason_ReturnsBadRequest:** Seed data and set the notification to `Completed`. Call `PUT /api/VesselVisitNotifications/{id}/reject` with an empty `Reason`. Expect `400 Bad Request` and response body containing the domain validation message: "A rejection reason must be provided.".

* **ApproveWithoutDock_ReturnsBadRequest:** Seed data and set the notification to `Completed`. Call `PUT /api/VesselVisitNotifications/{id}/approve` with an empty `DockId`. Expect `400 Bad Request` and response body containing the domain validation message: "A dock must be assigned when approving a notification.".

* **DecisionOnAlreadyReviewed_PreventsDuplicateDecisions:** Seed data and set the notification to `Completed`. Call `PUT /.../{id}/approve` with a valid `{ DockId, OfficerId }` → expect `200 OK`. Then call the same approve endpoint again → expect `400 Bad Request`. Also calling `PUT .../{id}/reject` after an approval should return `400 Bad Request` (duplicate decision prevented).


**Validation Criteria:**

* Correct HTTP status codes: `200 OK` for success, `400 Bad Request` for business rule violations.
* Data persistence is consistent after operations and retrievable via GET endpoints.

**Expected Outcome:**  
Integration tests confirm orchestration: controller → service → repository → unit of work.

---

#### **4.3 System Tests – `VesselVisitNotificationSystemTests.cs`**

**Purpose:**  
Test end-to-end workflows via the HTTP API, simulating real user actions across the full lifecycle.

**Test Descriptions:**

* **RejectAfterCompleted_AllowsRejection_System:** End-to-end: create vessel type and vessel via API, seed organization/representative and a notification in the app DB, set the notification `Status` to `Completed` in the DB, call `PUT /api/VesselVisitNotifications/{id}/reject` with `{ Reason, OfficerId }`. Expect `200 OK`, returned `status` = `Rejected` and `rejectedReason` matches provided value.

* **RejectWithoutReason_ReturnsBadRequest_System:** End-to-end: same seeding and set-to-`Completed`. Call `PUT /api/VesselVisitNotifications/{id}/reject` with empty `Reason`. Expect `400 Bad Request` and response body containing "A rejection reason must be provided.".

* **ApproveWithoutDock_ReturnsBadRequest_System:** End-to-end: seed and set `Completed`. Call `PUT /api/VesselVisitNotifications/{id}/approve` with empty `DockId`. Expect `400 Bad Request` and response body containing "A dock must be assigned when approving a notification.".

* **DecisionOnAlreadyReviewed_PreventsDuplicateDecisions_System:** End-to-end: seed and set `Completed`. Call `PUT .../{id}/approve` with `{ DockId, OfficerId }` → expect `200 OK`. Calling the same approve endpoint again should return `400 Bad Request`. Calling `PUT .../{id}/reject` after an approval should also return `400 Bad Request` (prevent duplicate decisions).


**Validation Criteria:**

* End-to-end HTTP flows return expected status codes and payloads.
* Persistence layer stores and returns the updated state/data.

**Expected Outcome:**  
System tests confirm the full lifecycle works via the API.

---

#### **4.4 Application Tests – `VesselVisitNotificationApplicationTests.cs`**

**Purpose:**  
Exercise the application/service layer using in-memory repositories and a test unit-of-work to validate orchestration, persistence calls and UoW commits without starting the HTTP host.

**Test Descriptions:**

* **ApproveAfterCompleted_AllowsApproval:** Verify that when a persisted notification is set to `Completed` (test sets this via reflection), calling `ApproveAsync` on the service.

* **ApproveWithValidDockAssignment_ChangesStatusAndStoresDock:** Verify that a notification set to `Completed` can be approved via `ApproveAsync`, status becomes `Approved`, and `AssignedDock` is stored.

* **RejectWithValidReason_ChangesStatusAndStoresReason:** Verify that a `Completed` notification can be rejected via `RejectAsync`, status becomes `Rejected`, and `RejectedReason` is stored.

* **RejectWithoutReason_ThrowsValidationException:** Verify rejecting without a reason fails with `BusinessRuleValidationException` and no extra commit occurs.

* **ApproveWithoutDockAssignment_ThrowsValidationException:** Verify approving without specifying a dock fails with `BusinessRuleValidationException` and no extra commit occurs.

* **DecisionOnAlreadyReviewedNotification_PreventsDuplicateDecisions:** Verify that approving/rejecting an already reviewed notification is rejected by business rules (prevents duplicate decisions).

**Validation Criteria:**

* Service returns a domain DTO or aggregate with the updated `Status` and `AssignedDock`.
* The in-memory unit-of-work recorded the expected number of commits.

**Expected Outcome:**  
Application tests validate service-layer orchestration, repository interactions and unit-of-work behavior without running the full API host.

---

#### **4.5 Functional Tests**

* **Test Approve/Reject (when Completed):** Officer approves (assign dock) or rejects (provide reason); system records metadata and enforces validation.

---

**Conclusion:**  
The combination of **unit, integration, and system tests** provides comprehensive coverage for US2.2.7, ensuring the notification lifecycle, state transitions, parameter validation and persistence behave correctly.


