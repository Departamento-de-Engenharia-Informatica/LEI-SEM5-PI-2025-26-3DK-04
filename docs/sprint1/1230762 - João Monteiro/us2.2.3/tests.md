## 4. Tests

### 4.1. Unit Tests

#### 4.1.1. DockService Tests
- **Test Dock Creation with Valid Data:**
    - Validate that a new dock can be successfully created with valid input data.
    - Verify that the dock is assigned a unique ID.
    - Ensure the dock data is correctly persisted in the repository.

- **Test Dock Creation with Duplicate ID:**
    - Verify that creating a dock with an existing ID fails.
    - System should throw a validation error or reject the request.
    - Ensure no duplicate dock entries are persisted.

- **Test Vessel Type Associations:**
    - Validate that each dock can be correctly associated with one or more vessel types.
    - Ensure associations are persisted and retrievable.
    - Confirm that invalid vessel types are rejected.

- **Test Persistence of Dock Data:**
    - Verify that after saving, dock details (ID, name, capacity, vessel types) are correctly stored and retrievable.
    - Ensure data consistency between service and database layers.

### 4.2. Functional Tests

- **Test Dock Registration via UI:**
    - User navigates to the “Register Dock” page.
    - User enters valid dock details and submits.
    - System confirms creation and displays a success message.
    - Verify the new dock appears in the dock list.

- **Test Dock Registration without Vessel Types:**
    - User attempts to register a dock without selecting any vessel types.
    - System displays an appropriate error message.
    - Dock is not created.

- **Test Duplicate Dock ID via UI:**
    - User attempts to register a dock with an existing dock ID.
    - System prevents creation and displays an error message.

- **Test Dock List Display:**
    - User opens the dock management page.
    - Verify that all existing docks are displayed with correct information (ID, name, vessel types).
