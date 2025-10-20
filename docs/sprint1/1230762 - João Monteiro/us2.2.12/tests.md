## 4. Tests

### 4.1. Unit Tests

#### 4.1.1. ResourceService Tests
- **Test Resource Creation with Valid Data:**
    - Validate that a new physical resource can be successfully created with valid input data.
    - Verify that mandatory fields (resource code, name, qualification, setup time) are correctly stored.
    - Ensure that the resource is initialized with an “Active” status.

- **Test Qualification Requirements:**
    - Verify that the system correctly enforces qualification requirements for each resource type.
    - Attempt to register a resource without the necessary qualification and ensure validation fails.
    - Confirm that valid qualification data allows successful resource creation.

- **Test Setup Time Storage and Usage:**
    - Validate that the setup time value is correctly stored during resource creation.
    - Ensure that the stored setup time is accurately used in scheduling and operation assignments.
    - Confirm data consistency between the service and persistence layers.

- **Test Status Transitions and Historical Data Retention:**
    - Verify that deactivating a resource updates its status from “Active” to “Inactive”.
    - Ensure that all historical data (operations performed, timestamps, qualifications) is preserved after deactivation.
    - Confirm that reactivation or further edits are handled according to business rules.

### 4.2. Functional Tests

- **Test Resource Registration via UI:**
    - User navigates to the “Register Physical Resource” page.
    - User enters valid resource details and submits.
    - System confirms successful creation and displays a success message.
    - Verify that the new resource appears in the resource list with correct data.

- **Test Duplicate Resource Code via UI:**
    - User attempts to register a resource with a code that already exists.
    - System blocks submission and displays an appropriate error message.
    - Ensure that no duplicate resource is created in the system.

- **Test Resource Deactivation via UI:**
    - User selects an existing resource and performs a deactivation action.
    - System updates the resource status to “Inactive” while preserving all historical data.
    - Verify that deactivated resources remain visible in the system with their activity logs intact.

- **Test Resource List Display:**
    - User accesses the resource management page.
    - Verify that all active and inactive resources are displayed with correct attributes (code, qualification, setup time, status).
