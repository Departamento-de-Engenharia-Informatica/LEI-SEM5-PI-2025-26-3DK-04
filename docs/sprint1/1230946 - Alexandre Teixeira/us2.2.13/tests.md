## 4. Tests

### 4.1. Unit Tests

#### 4.1.1. QualificationService Tests
- **Test Create Qualification with Valid Data:**
  - Validate that a qualification can be created with a unique code and descriptive name.
  - Verify the qualification code is unique.
  - Ensure both code and name are correctly stored.
  
- **Test Create Qualification with Duplicate Code:**
  - Verify that creating a qualification with an existing code fails.
  - System should display appropriate error message (e.g., "Qualification code already exists").

- **Test Create Qualification with Missing Required Fields:**
  - Verify that creating a qualification without a code fails.
  - Verify that creating a qualification without a name fails.
  - System should validate all mandatory fields.

- **Test Update Qualification Data:**
  - Validate that qualification descriptive name can be updated.
  - Verify the qualification code cannot be changed (immutable identifier).
  - Ensure updated name is correctly persisted.

- **Test Update Qualification with Empty Name:**
  - Verify that updating a qualification with an empty or null name fails.
  - System should enforce name as a required field.

#### 4.1.2. Qualification Entity Tests
- **Test Qualification Entity Creation:**
  - Validate that Qualification entity is created with valid code and name.
  - Verify code format validation (e.g., no special characters, max length).
  - Verify name format validation (e.g., non-empty, max length).

- **Test Qualification Code Immutability:**
  - Ensure that once created, the qualification code cannot be modified.
  - Code serves as the primary identifier.

#### 4.1.3. Authorization Tests
- **Test Operator Authorization:**
  - Verify only authenticated Logistics Operators can manage qualifications.
  - Ensure other user roles cannot create/update qualifications.

#### 4.1.4. Search and Filter Tests
- **Test Search by Qualification Code:**
  - Validate that qualifications can be searched by code.
  - Verify exact match returns correct result.

- **Test Filter by Name:**
  - Validate that qualifications can be filtered by name.
  - Verify partial matches are supported (e.g., searching "Crane" returns "STS Crane Operator").

- **Test Combined Search:**
  - Validate that qualifications can be searched by code or name simultaneously.
  - Verify correct results are returned.

#### 4.1.5. Pre-existence Validation Tests
- **Test Assignment to Staff Member:**
  - Verify that only existing qualifications can be assigned to staff members.
  - Attempting to assign a non-existent qualification should fail with appropriate error.

- **Test Assignment to Resources:**
  - Verify that only existing qualifications can be assigned to resources.
  - System should validate qualification existence before assignment.

### 4.2. Functional Tests

- **Test Create Qualification via UI:**
  - Operator logs in and navigates to qualification management.
  - Operator enters qualification code and descriptive name (e.g., "STS_CRANE", "STS Crane Operator").
  - System validates data and creates the qualification.
  - System displays success message.
  - Qualification appears in the list.

- **Test Create Duplicate Qualification via UI:**
  - Operator attempts to create a qualification with an existing code.
  - System detects duplicate code.
  - System displays error message: "Qualification code already exists".
  - Qualification is not created.

- **Test Update Qualification via UI:**
  - Operator selects an existing qualification.
  - Operator updates the descriptive name (e.g., from "Truck Driver" to "Heavy Truck Driver").
  - System validates and saves the changes.
  - System displays success message.
  - Updated name is reflected in the system.
  - Qualification code remains unchanged.

- **Test Search Qualification by Code via UI:**
  - Operator enters a qualification code in the search field.
  - System displays the matching qualification.
  - Search is case-insensitive.

- **Test Filter Qualifications by Name via UI:**
  - Operator enters a partial name in the filter field (e.g., "Crane").
  - System displays all qualifications containing "Crane" in the name.
  - Filter supports partial matching.

- **Test View All Qualifications via UI:**
  - Operator navigates to qualification list.
  - System displays all qualifications with code and name.
  - List is sortable by code or name.

- **Test Qualification Pre-existence Check:**
  - When assigning qualifications to staff or resources, only existing qualifications are available in dropdown/selection.
  - System prevents manual entry of non-existent qualification codes.