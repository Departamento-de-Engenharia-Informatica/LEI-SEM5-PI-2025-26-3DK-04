## 4. Tests

### 4.1. Unit Tests

#### 4.1.1. StorageAreaService Tests
- **Test Storage Area Creation with Valid Data:**
    - Validate that a new storage area can be successfully created with valid input data.
    - Verify that the storage area is assigned a unique identifier.
    - Ensure the storage area data is correctly persisted in the repository.

- **Test Dock Associations and Distance Metadata:**
    - Validate that each storage area can be correctly associated with one or more docks.
    - Ensure distance metadata between the storage area and associated docks is accurately stored.
    - Confirm that invalid dock associations are rejected.

- **Test Update Within Capacity Limits:**
    - Verify that updating storage area occupancy within the defined capacity succeeds.
    - Ensure that data integrity is maintained after the update.

- **Test Update Beyond Capacity:**
    - Verify that attempting to update a storage area with occupancy exceeding its maximum capacity fails.
    - System should display or log an appropriate validation error message.

- **Test Persistence of Storage Area Data:**
    - Confirm that all storage area details (ID, name, capacity, associated docks, distance data) are correctly saved and retrievable.
    - Ensure consistency between service layer and database.

### 4.2. Functional Tests

- **Test Storage Area Registration via UI:**
    - User navigates to the “Register Storage Area” page.
    - User enters valid storage area details and submits.
    - System confirms creation and displays a success message.
    - Verify that the new storage area appears in the storage area list.

- **Test Update Occupancy Beyond Capacity via UI:**
    - User attempts to update a storage area’s occupancy above its maximum limit.
    - System prevents the update and displays an appropriate error message.

- **Test Dock Association Management via UI:**
    - User assigns docks to a storage area.
    - System validates associations and saves them successfully.
    - Verify that the updated storage area details reflect correct dock associations.

- **Test Storage Area List Display:**
    - User accesses the storage area management page.
    - Verify that all existing storage areas are displayed with correct capacity and occupancy details.
