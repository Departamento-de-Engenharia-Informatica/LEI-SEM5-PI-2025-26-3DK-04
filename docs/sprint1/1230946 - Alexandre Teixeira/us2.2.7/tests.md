## 4. Tests

### 4.1. Unit Tests

#### 4.1.1. VesselVisitNotificationService Tests
- **Test Approval with Valid Dock Assignment:**
  - Validate that a pending notification can be approved with a valid dock assignment.
  - Verify the notification status changes to "Approved".
  - Ensure the assigned dock is correctly stored.
  
- **Test Rejection with Valid Reason:**
  - Validate that a pending notification can be rejected with a valid rejection reason.
  - Verify the notification status changes to "Rejected".
  - Ensure the rejection reason is correctly stored.

- **Test Rejection without Reason:**
  - Verify that rejecting a notification without providing a reason fails.
  - System should display appropriate error message.

- **Test Approval without Dock Assignment:**
  - Verify that approving a notification without assigning a dock fails.
  - System should display appropriate error message.

- **Test Decision on Already Reviewed Notification:**
  - Ensure that attempting to approve/reject an already reviewed notification is rejected.
  - System should prevent duplicate decisions.

#### 4.1.2. DecisionAuditLog Tests
- **Test Audit Log Creation:**
  - Validate that each decision (approve/reject) creates an audit log entry.
  - Verify timestamp is automatically recorded.
  - Ensure officer ID is correctly captured.
  - Confirm decision outcome (approved/rejected) is logged.

- **Test Audit Log Immutability:**
  - Verify that audit log entries cannot be modified after creation.
  - Ensure audit trail integrity.

#### 4.1.3. Authorization Tests
- **Test Officer Authorization:**
  - Verify only authenticated Port Authority Officers can review notifications.
  - Ensure other user roles (e.g., Shipping Agents) cannot approve/reject notifications.

- **Test Pending Notifications Retrieval:**
  - Validate that only pending (not yet reviewed) notifications are displayed.
  - Ensure approved/rejected notifications are excluded from the list.

### 4.2. Functional Tests

- **Test Approval via UI:**
  - Officer logs in and navigates to pending notifications.
  - Officer selects a notification and chooses "Approve".
  - Officer assigns a dock from available docks.
  - System confirms approval and displays success message.
  - Verify audit log is visible with timestamp and officer details.

- **Test Rejection via UI:**
  - Officer logs in and navigates to pending notifications.
  - Officer selects a notification and chooses "Reject".
  - Officer provides rejection reason (e.g., "Missing cargo information").
  - System confirms rejection and displays success message.
  - Verify audit log is visible with timestamp and officer details.

- **Test Empty Pending List:**
  - When no pending notifications exist, system displays appropriate message.
  - Officer cannot make decisions when list is empty.

- **Test Filter and Search:**
  - Officer can filter pending notifications by vessel, date, or shipping agent.
  - Search functionality returns correct results.