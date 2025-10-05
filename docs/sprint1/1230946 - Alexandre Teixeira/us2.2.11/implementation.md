# 5. User Story Implementation Report

---

## 5.1. US_2.2.11 – Register and Manage Operating Staff Members

### 5.2. Description

As a Logistics Operator, I want to register and manage operating staff members (create, update, deactivate), so that the system can accurately reflect staff availability and ensure that only qualified personnel are assigned to resources during scheduling.

### 5.3. Implementation Details

- **User Interaction & Flow Control**  
  A new UI (`ManageStaffMemberUI`) was developed to allow Logistics Operators to create, update, and deactivate operating staff members. The interface provides forms for data entry and displays staff members with search and filter capabilities.

- **Business Logic Coordination**  
  The `StaffMemberController` manages the staff management process and delegates validation, business rule enforcement, and persistence to the application service.

- **Domain Entity Construction**  
  The `OperatingStaffMember` entity encapsulates all staff member data including:
  - Unique mecanographic number (ID)
  - Short name
  - Contact details (email and phone with format validation)
  - Qualifications
  - Operational window (availability schedule)
  - Current status (available, unavailable, etc.)
  
  Value objects such as `MecanographicNumber`, `ContactDetails`, `Qualifications`, `OperationalWindow`, and `StaffStatus` ensure data integrity and encapsulation.

- **Persistence**  
  The `IStaffMemberRepository` interface and its implementation handle staff member data persistence. The repository includes methods to:
  - Save new staff members (create)
  - Update existing staff member data
  - Deactivate staff members (soft delete - preserves data)
  - Reactivate staff members
  - Search and filter by mecanographic number, name, status, and qualifications

- **Data Preservation**  
  Deactivation does not delete staff data from the database. Instead, it updates the status field to "inactive" or "unavailable", preserving all information for audit and historical planning purposes. Reactivation simply updates the status back to "available" without any data loss.

- **Authorization & Security**  
  Role-based access control ensures that only authenticated users with the "Logistics Operator" role can create, update, or deactivate staff members.

- **Validation & Business Rules**  
  The system enforces:
  - Unique mecanographic number validation (prevents duplicates)
  - Email format validation
  - Phone number format validation
  - Required fields validation (all mandatory fields must be provided)
  - Qualification validation (from predefined set)

- **Search & Filter Capabilities**  
  Staff members can be searched and filtered by:
  - Mecanographic number (ID) - exact match
  - Name - partial match supported
  - Status - filter by available/unavailable
  - Qualifications - filter by specific qualifications

---