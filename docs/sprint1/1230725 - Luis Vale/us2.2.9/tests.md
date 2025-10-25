### 4. Unit, Integration, and System Tests

**User Story:** US2.2.9 - *Change/Complete a Vessel Visit Notification*

---

#### **4.1 Unit Tests – `VesselVisitNotificationTests.cs`**

**Purpose:**  
Ensure that the business rules of the `VesselVisitNotification` aggregate function correctly, validating updates, submission, withdrawal, resumption, and state constraints.

**Test Descriptions:**

* **UpdateInProgress_WithNullBothCargo_Throws:** verifies that at least one field must be provided for updating a notification; otherwise, an exception is thrown.
* **UpdateInProgress_WhenNotInProgress_Throws:** prevents updates on notifications that are not in progress.
* **SubmitForApproval_OnlyFromInProgress:** ensures that only in-progress notifications can be submitted.
* **Withdraw_OnlyFromInProgress:** ensures that only in-progress notifications can be withdrawn.
* **Resume_OnlyFromWithdrawn:** allows resuming only notifications that were previously withdrawn.

**Validation Criteria:**

* Domain rules are strictly enforced.
* Updates affect only the allowed fields.
* States (`InProgress`, `Submitted`, `WithdrawnRequest`) remain consistent and valid.

**Expected Outcome:**  
All tests pass, ensuring that `VesselVisitNotification` respects business rules for each state transition and update.

---

#### **4.2 Integration Tests – `VesselVisitNotificationIntegrationTests.cs`**

**Purpose:**  
Verify correct interaction between layers (service, repository, API) when updating, submitting, withdrawing, or resuming vessel visit notifications.

**Test Descriptions:**

* **SubmitEndpoint_ChangesStatusToSubmitted:** validates that submission via API changes status to `Submitted`.
* **WithdrawAndResumeFlow_Works:** checks that withdraw (`Withdraw`) and resume (`Resume`) work correctly through HTTP endpoints.
* **UpdateInProgress_UpdatesCargoSuccessfully:** ensures that changes to `LoadingCargo` and `UnloadingCargo` are applied correctly via API.

**Validation Criteria:**

* Correct HTTP status codes: `200 OK`, `400 Bad Request`.
* Data persistence is consistent after operations.
* Fields and containers are updated correctly.

**Expected Outcome:**  
The API correctly processes updates, submission, withdrawal, and resumption, respecting state constraints.

---

#### **4.3 System Tests – `VesselVisitNotificationSystemTests.cs`**

**Purpose:**  
Test the complete end-to-end workflow of a vessel visit notification, from creation to submission, withdrawal, and update, simulating real user interactions via API.

**Test Descriptions:**

* **Submit_Withdraw_Resume_Flow_Works:**
    1. Create vessel types, vessel, and representative.
    2. Create a notification directly in the database.
    3. Submit the notification via API, confirming status change to `Submitted`.

* **WithdrawAndResumeFlow_Works_EndToEnd:**
    1. Create a new notification in `InProgress`.
    2. Execute `Withdraw` via API and validate status changes to `WithdrawnRequest`.
    3. Execute `Resume` and validate status returns to `InProgress`.

* **UpdateInProgress_SystemTest_WorksEndToEnd:**
    1. Create a notification with loading and unloading cargo manifests.
    2. Update the notification via API, modifying manifests and containers.
    3. Confirm that data was updated and status remains `InProgress`.

**Validation Criteria:**

* HTTP workflows follow expected business behavior.
* Created and updated data are persistent and retrievable.
* Invalid operations are rejected according to business rules.

**Expected Outcome:**  
The system supports the full lifecycle of vessel visit notifications, ensuring integrity, consistent status, and domain validations.

---

#### **4.4 Functional Tests**

- **Test Edit In-Progress Notification via UI:**
    - Representative opens an “InProgress” Vessel Visit Notification.
    - Updates vessel name and arrival date.
    - System confirms update and maintains status “InProgress”.

- **Test Submit Notification via UI:**
    - Representative clicks “Submit for Approval”.
    - System validates all required fields, updates status to “Submitted”, and displays confirmation.

- **Test Edit After Submission:**
    - Representative tries to modify a “Submitted” notification.
    - System blocks edit and displays error “Submitted notifications cannot be modified”.

- **Test Withdraw Notification:**
    - Representative cancels an “InProgress” notification.
    - System confirms cancellation and removes it from active list.

- **Test Validation Errors:**
    - Representative attempts to submit notification missing mandatory fields.
    - System returns descriptive validation messages.


---

**Conclusion:**  
The combination of **unit, integration, and system tests** ensures that US2.2.9 – *Change/Complete a Vessel Visit Notification* is fully covered, encompassing business rules, data persistence, and end-to-end API and UI workflows.

---





