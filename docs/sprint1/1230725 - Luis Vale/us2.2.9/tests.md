
## 4. Tests

### 4.1. Unit Tests

- **Test Update In-Progress Notification:**
    - Validate that an “InProgress” notification can be edited successfully.
    - Ensure updates persist and status remains “InProgress”.

- **Test Submit Notification:**
    - Verify that submitting an “InProgress” notification changes status to “Submitted”.
    - Confirm submission timestamp is generated.

- **Test Submit Already Submitted Notification:**
    - Attempt to submit a notification that’s already in “Submitted” status.
    - System should reject with **400 Bad Request**.

- **Test Edit After Submission:**
    - Attempt to edit a “Submitted” notification.
    - System should block modifications and return appropriate error.

- **Test Cancel In-Progress Notification:**
    - Verify that canceling an “InProgress” notification removes or marks it as withdrawn.
    - Confirm status change is correctly reflected.

---

### 4.2. Functional Test


#### US 2.2.9 - Edit or Submit Vessel Visit Notification
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


