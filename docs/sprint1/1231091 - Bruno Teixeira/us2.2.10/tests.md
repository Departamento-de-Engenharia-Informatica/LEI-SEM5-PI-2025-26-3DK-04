### 4. Unit, Integration and System Tests

**User Story:** US2.2.10 - *View Status of Submitted Vessel Visit Notifications*

---

#### **4.1 Unit Tests – `VesselVisitNotificationApplicationTests.cs`**

**Objective:**
Verify the correct behavior of the search and filter functionality in `VesselVisitNotificationService`, ensuring that shipping agent representatives can view and filter their submitted notifications by various criteria including vessel, status, representative, and time period.

**Test Descriptions:**

* **SearchNotifications_WithoutFilters_ReturnsAllNotifications:** validates that performing a search without any filters returns all notifications accessible to the user.
* **SearchNotifications_FilterByVessel_ReturnsMatch:** confirms that filtering by a specific vessel ID returns only notifications for that vessel.
* **SearchNotifications_FilterByStatus_ReturnsMatchingNotifications:** verifies that filtering by status (In Progress, Pending, Approved, Rejected) returns only notifications with the specified status.
* **SearchNotifications_FilterByRepresentative_ReturnsOnlyTheirNotifications:** ensures that filtering by representative ID returns only notifications submitted by that representative.
* **SearchNotifications_FilterByDateRange_ReturnsNotificationsInRange:** validates that filtering by a date range (start and end dates) returns only notifications created within that period.
* **SearchNotifications_FilterByStartDateOnly_ReturnsNotificationsAfterDate:** confirms that filtering with only a start date returns notifications created on or after that date.
* **SearchNotifications_FilterByEndDateOnly_ReturnsNotificationsBeforeDate:** verifies that filtering with only an end date returns notifications created on or before that date.
* **SearchNotifications_CombinedFilters_ReturnsMatchingNotifications:** ensures that applying multiple filters simultaneously (vessel, status, representative, date range) correctly returns notifications matching ALL criteria.
* **SearchNotifications_FilterByMultipleStatuses_ReturnsMatchingNotifications:** validates that filtering by multiple status values returns notifications with any of the specified statuses.
* **SearchNotifications_NoMatchingFilters_ReturnsEmptyList:** confirms that applying filters that match no notifications returns an empty list without errors.
* **SearchNotifications_ApprovedStatus_IncludesDockAssignment:** verifies that notifications with "Approved" status include dock assignment information in the response.
* **SearchNotifications_RejectedStatus_IncludesRejectionReason:** ensures that notifications with "Rejected" status include the rejection reason provided by the Port Authority.

**Validation Criteria:**

* Filter logic correctly implements AND/OR operations as required.
* Date range filtering accurately compares timestamps.
* Status filtering supports single and multiple status values.
* Approved notifications contain dock assignment details.
* Rejected notifications contain rejection reasons.
* Empty filter sets return all accessible notifications.
* No exceptions are thrown for valid filter combinations.

**Expected Result:**
All tests pass successfully, ensuring that shipping agent representatives can effectively search and filter their vessel visit notifications by multiple criteria.

---

#### **4.2 Integration Tests – `VesselVisitNotificationIntegrationTests.cs`**

**Objective:**
Validate communication between the application layer, API controller, and database (`DDDSample1DbContext`), ensuring that search and filter operations work correctly through the REST API with proper authorization and data access controls.

**Test Descriptions:**

* **Search_WithoutFilters_ReturnsAllAccessibleNotifications:** confirms that `GET /api/VesselVisitNotifications/search` without parameters returns all notifications the representative has access to.
* **Search_FilterByVessel_ReturnsCorrectNotifications:** verifies that filtering by vessel ID via query parameters returns the correct subset.
* **Search_FilterByStatus_ReturnsMatchingNotifications:** validates that status filtering works correctly through the API.
* **Search_FilterByRepresentative_RespectsAccessControl:** ensures that representatives can only see notifications from their organization.
* **Search_FilterByDateRange_ReturnsNotificationsInRange:** confirms that date range filtering works correctly with ISO 8601 date formats.
* **Search_CombinedFilters_WorksCorrectly:** validates that multiple query parameters can be combined effectively.
* **Search_ApprovedNotification_IncludesDockInfo:** verifies that approved notifications return dock assignment data in the JSON response.
* **Search_RejectedNotification_IncludesReason:** ensures that rejected notifications include the rejection reason in the response.
* **Search_UnauthorizedAccess_ReturnsForbidden:** validates that representatives cannot access notifications from other organizations (returns *403 Forbidden*).

**Validation Criteria:**

* Correct HTTP response codes (`200`, `400`, `403`, `404`).
* JSON content matches the `VesselVisitNotificationDto` model.
* Query parameters are correctly parsed and applied.
* Access control rules are enforced (organization-level visibility).
* Date formats are correctly handled (ISO 8601).
* Dock and rejection reason fields are populated when applicable.

**Expected Result:**
All API endpoints behave as expected, respecting access control, filter logic, and data visibility rules.

---

#### **4.3 System Tests – `VesselVisitNotificationSystemTests.cs`**

**Objective:**
Validate the complete workflow of viewing and filtering vessel visit notifications through the HTTP API, simulating real shipping agent representative behavior and demonstrating the full notification lifecycle from multiple perspectives.

**Test Descriptions:**

* **VesselVisitNotification_Search_Full_Workflow_via_HttpApi:**

    1. Creates an organization with representatives via `POST /api/Organizations`.
    2. Creates vessels and vessel types via `POST /api/VesselTypes` and `POST /api/Vessels`.
    3. Creates multiple vessel visit notifications with different statuses via `POST /api/VesselVisitNotifications`.
    4. Representative searches without filters via `GET /api/VesselVisitNotifications/search`.
    5. Verifies all their notifications are returned.
    6. Filters by specific vessel via `GET /api/VesselVisitNotifications/search?vesselId={id}`.
    7. Verifies only relevant notifications are returned.
    8. Filters by status "Pending" via `GET /api/VesselVisitNotifications/search?status=Pending`.
    9. Filters by date range via `GET /api/VesselVisitNotifications/search?startDate={date}&endDate={date}`.
    10. Applies combined filters (vessel + status + date range).
    11. Port Authority approves a notification with dock assignment via `PATCH /api/VesselVisitNotifications/{id}/approve`.
    12. Representative searches for approved notifications and verifies dock assignment is included.
    13. Port Authority rejects a notification with reason via `PATCH /api/VesselVisitNotifications/{id}/reject`.
    14. Representative searches for rejected notifications and verifies rejection reason is included.
    15. Attempts to access another organization's notifications, expecting *403 Forbidden*.

**Validation Criteria:**

* The system maintains consistency between HTTP calls.
* Search results reflect the current state of notifications.
* Filters work correctly in combination.
* Responses and status codes correctly reflect business rules.
* Access control prevents cross-organization data access.
* Approved notifications show dock assignments.
* Rejected notifications show rejection reasons.
* Date range filtering is accurate and handles edge cases.

**Expected Result:**
The complete workflow executes successfully, demonstrating that shipping agent representatives can effectively view and filter their vessel visit notifications, with proper status information, dock assignments, and rejection reasons, while respecting organization-level access control.

### 4.4. Functional Tests

- **Test View All Submitted Notifications via UI:**
    - Shipping Agent Representative logs in and navigates to "My Notifications".
    - System displays all notifications submitted by the representative.
    - Each notification shows vessel name, status badge, submission date.
    - Verify notifications are displayed in a clear table or card format.

- **Test View Organization Notifications:**
    - Representative views notifications submitted by colleagues in the same organization.
    - System clearly indicates which notifications belong to the logged-in representative.
    - Verify notifications from other organizations are not visible.

- **Test Filter by Vessel:**
    - Representative selects a vessel from the filter dropdown.
    - System updates the list to show only notifications for that vessel.
    - Verify the filter can be cleared to show all notifications again.

- **Test Filter by Status:**
    - Representative selects a status filter (e.g., "Pending" or "Approved").
    - System displays only notifications with the selected status.
    - Status badges are color-coded for easy identification.
    - Verify multiple statuses can be selected simultaneously.

- **Test Filter by Representative:**
    - Representative (if authorized) filters by specific representative name.
    - System shows only notifications submitted by that person.
    - Verify proper access control for viewing others' notifications.

- **Test Filter by Date Range:**
    - Representative selects start and/or end dates using date picker.
    - System filters notifications created within the specified range.
    - Verify date pickers work correctly with various date formats.
    - Confirm filtered results match the selected date criteria.

- **Test View Approved Notification Details:**
    - Representative clicks on a notification with "Approved" status.
    - System displays detailed view including assigned dock information.
    - Dock details show: dock name, location, assignment date/time.
    - Approval timestamp is clearly visible.

- **Test View Rejected Notification Details:**
    - Representative clicks on a notification with "Rejected" status.
    - System displays the rejection reason provided by Port Authority.
    - Rejection reason is prominently displayed and easy to read.
    - Rejection timestamp is clearly visible.

- **Test Multiple Filter Combination:**
    - Representative applies multiple filters: vessel + status + date range.
    - System correctly applies all filters in combination (AND logic).
    - Result set matches all selected criteria.
    - Verify filters can be adjusted independently.

- **Test Clear All Filters:**
    - Representative applies various filters.
    - Representative clicks "Clear Filters" button.
    - System removes all filters and shows the full accessible list.
    - UI returns to default state.

- **Test Status Updates Reflection:**
    - Representative views a notification with "Pending" status.
    - Port Authority approves/rejects it (in separate session).
    - Representative refreshes the page or system auto-refreshes.
    - Updated status and additional information (dock/reason) are displayed.

- **Test Empty Results Message:**
    - Representative applies filters that match no notifications.
    - System displays a user-friendly "No notifications found" message.
    - Message suggests adjusting filters or checking back later.
    - No errors or blank pages are shown.

- **Test Notification Details Modal/Page:**
    - Representative clicks to view full details of any notification.
    - System shows comprehensive information: vessel details, cargo manifest, crew list, status history.
    - For approved: dock assignment and berthing schedule.
    - For rejected: detailed reason and suggested corrections.

---

**Conclusion:**
The tests performed across all three layers (unit, integration, and system) validate the full behavior of *User Story US2.2.10*, ensuring that shipping agent representatives can effectively view, search, and filter their vessel visit notifications with proper status information (including dock assignments for approved notifications and rejection reasons for rejected notifications), while respecting organization-level access control and data visibility rules.
