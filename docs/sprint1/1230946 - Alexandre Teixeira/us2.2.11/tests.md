## 4. Tests

### 4.1. Unit Tests

#### 4.1.1. StaffMemberService Tests
- **Test Create Staff Member with Valid Data:**
  - Validate that a staff member can be created with all required fields.
  - Verify the mecanographic number is unique.
  - Ensure all mandatory fields are correctly stored (short name, email, phone, qualifications, operational window, status).
  
- **Test Create Staff Member with Duplicate Mecanographic Number:**
  - Verify that creating a staff member with an existing mecanographic number fails.
  - System should display appropriate error message.

- **Test Create Staff Member with Missing Required Fields:**
  - Verify that creating a staff member without required fields (name, email, etc.) fails.
  - System should validate all mandatory fields.

- **Test Update Staff Member Data:**
  - Validate that staff member data can be updated (name, contact details, qualifications, operational window).
  - Verify the mecanographic number cannot be changed.
  - Ensure updated data is correctly persisted.

- **Test Deactivate Staff Member:**
  - Verify that a staff member can be deactivated.
  - Ensure the status changes to "unavailable" or "inactive".
  - Confirm staff data is preserved (not deleted).

- **Test Reactivate Staff Member:**
  - Validate that a deactivated staff member can be reactivated.
  - Verify the status changes back to "available".
  - Ensure no data loss occurred during deactivation.

#### 4.1.2. OperatingStaffMember Tests
- **Test Staff Member Entity Creation:**
  - Validate that OperatingStaffMember entity is created with valid data.
  - Verify email format validation.
  - Verify phone number format validation.
  - Ensure qualifications are from a valid set.

- **Test Status Management:**
  - Verify status transitions (available ↔ unavailable).
  - Ensure invalid status values are rejected.

#### 4.1.3. Authorization Tests
- **Test Operator Authorization:**
  - Verify only authenticated Logistics Operators can manage staff members.
  - Ensure other user roles cannot create/update/deactivate staff.

#### 4.1.4. Search and Filter Tests
- **Test Search by Mecanographic Number:**
  - Validate that staff members can be searched by ID.
  - Verify exact match returns correct result.

- **Test Filter by Name:**
  - Validate that staff members can be filtered by name.
  - Verify partial matches are supported.

- **Test Filter by Status:**
  - Validate that staff members can be filtered by status (available, unavailable).
  - Verify correct results are returned.

- **Test Filter by Qualifications:**
  - Validate that staff members can be filtered by qualifications.
  - Verify staff with matching qualifications are returned.

### 4.3. Functional Tests

- **Test Create Staff Member via UI:**
  - Operator logs in and navigates to staff management.
  - Operator enters all required information (mecanographic number, name, email, phone, qualifications, operational window).
  - System validates data and creates the staff member.
  - System displays success message.
  - Staff member appears in the list.

- **Test Update Staff Member via UI:**
  - Operator selects an existing staff member.
  - Operator updates contact details or qualifications.
  - System validates and saves the changes.
  - System displays success message.
  - Updated information is reflected in the system.

- **Test Deactivate Staff Member via UI:**
  - Operator selects an active staff member.
  - Operator chooses "Deactivate" option.
  - System changes status to inactive/unavailable.
  - Staff member is preserved in the database.
  - System displays confirmation message.

- **Test Reactivate Staff Member via UI:**
  - Operator selects a deactivated staff member.
  - Operator chooses "Reactivate" option.
  - System changes status back to available.
  - System displays confirmation message.

- **Test Search and Filter via UI:**
  - Operator can search staff by mecanographic number.
  - Operator can filter staff by name, status, or qualifications.
  - Search/filter results are displayed correctly.
  - Operator can clear filters to view all staff members.