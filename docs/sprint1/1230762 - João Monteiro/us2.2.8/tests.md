## 4. Tests

### 4.1. Unit Tests

#### 4.1.1. VesselVisitNotificationService Tests
- **Test Notification Creation with Valid Data:**
    - Validate that a new Vessel Visit Notification can be successfully created with valid vessel, cargo, and crew data.
    - Verify that all mandatory fields (vessel name, IMO number, cargo list, ETA) are correctly stored.
    - Ensure the notification status is initialized as "In Progress".

- **Test Container ID Format Validation:**
    - Verify that all container IDs comply with the ISO 6346 standard.
    - Attempt to create a notification with an invalid container ID and ensure the system raises a validation error.
    - Confirm that valid container IDs are accepted without issues.

- **Test Hazardous Cargo Safety Requirement:**
    - Validate that including hazardous cargo in the notification automatically requires assigning a Safety Officer in the crew list.
    - Ensure the system prevents submission when hazardous cargo is declared without a Safety Officer.
    - Confirm the appropriate error message is displayed or logged.

- **Test Status Transition from "In Progress" to "Submitted":**
    - Verify that once all required data is provided, the status of the notification transitions from "In Progress" to "Submitted".
    - Ensure the transition occurs only after successful validation of all fields.
    - Confirm that the final state is persisted correctly in the repository.

### 4.2. Functional Tests

- **Test Complete Notification Submission via UI:**
    - User logs in as a Shipping Agent.
    - User navigates to the “Create Vessel Visit Notification” page.
    - User fills in all required vessel, cargo, and crew data and submits.
    - System confirms successful submission and displays a success message.
    - Verify that the notification appears in the “Submitted Notifications” list.

- **Test Invalid Container ID Submission via UI:**
    - User attempts to submit a notification with an invalid container ID.
    - System blocks submission and displays an appropriate error message.
    - Verify that the notification is not persisted.

- **Test Hazardous Cargo without Safety Officer via UI:**
    - User enters cargo details including hazardous materials but omits assigning a Safety Officer.
    - System prevents submission and displays a validation error indicating the missing crew role.
    - Verify that the notification remains in “In Progress” status.

- **Test Automatic Status Update:**
    - User completes all required fields and submits.
    - System automatically updates the notification status to “Submitted”.
    - Confirm the updated status and timestamp are visible in the user interface.
