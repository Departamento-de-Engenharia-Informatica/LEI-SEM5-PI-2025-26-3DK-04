# 5. User Story Implementation Report

---

## 5.1. US_2.2.12 – Register Physical Resources

### 5.2. Description

As a Logistics Operator, I want to register and manage physical resources so that they can be accurately considered during planning, scheduling, and operational execution within the port environment.

---

### 5.3. Implementation Details

- **User Interaction & Flow Control**  
  A new UI (`RegisterResourceUI`) was developed to allow Logistics Operators to create and update physical resources. The interface enables users to input details such as resource code, name, type, qualification requirements, setup time, and operational status.

- **Business Logic Coordination**  
  The `RegisterResourceController` orchestrates the registration and update workflow. It validates user input via the `RegisterResourceService`, ensuring that business rules — such as unique resource codes, valid qualification requirements, and non-negative setup times — are enforced before persistence.

- **Domain Entity Construction**  
  The `PhysicalResource` entity encapsulates the main attributes of a resource, including code, name, type, qualification requirements, setup time, and operational status. It defines the core validation logic for data integrity and supports controlled state transitions (e.g., activation/deactivation).

- **Persistence**  
  The `IResourceRepository` interface and its implementation (`ResourceRepositorySQL`) handle persistence of physical resource data. This includes saving new resources, updating existing records, and managing associations with relevant operational data in the database.

- **Validation & Error Handling**  
  Validation rules ensure:
    - Unique resource codes across the system.
    - Presence of required qualification data for each resource type.
    - Correct handling of setup time values (must be greater than or equal to zero).  
      In case of validation failures, the system prevents persistence and displays descriptive error messages to guide the user.

- **Authorization & Security**  
  Role-based access control ensures that only authenticated users with the “Logistics Operator” role can register, modify, or deactivate physical resources. Unauthorized users are restricted from performing these actions.

- **Status Management & Historical Retention**  
  Each resource maintains an operational status (Active/Inactive). When a resource is deactivated, its historical data — including setup times, qualification info, and past operation references — is preserved to ensure traceability and auditability.

- **Data Consistency**  
  All resource registration and modification operations are performed within transactional boundaries to maintain database integrity. In case of validation or persistence errors, the transaction is rolled back to avoid inconsistent states.

---
