# US_2.2.12 – Register and Manage Physical Resources

## 1. Requirements

### 1.1. User Story

**As a Logistics Operator**,  
I want to register and manage physical resources (create, update, deactivate)  
so that they can be accurately considered during planning and scheduling operations.

---

### 1.2. Customer Specifications and Clarifications

**From the specification document:**

- “As a Logistics Operator, I want to register and manage physical resources (create, update, deactivate), so that they can be accurately considered during planning and scheduling operations.”
- Resources include cranes (fixed and mobile), trucks, and other equipment directly involved in vessel and yard operations.
- Each resource must have a unique alphanumeric code and a description.
- Each resource must store its operational capacity, which varies according to the kind of resource.
- Additional properties must include:
    - Current availability status (active, inactive, under maintenance)
    - Setup time (in minutes), if relevant
    - Qualification requirements (ensuring only properly certified staff can be scheduled with the resource)
- Deactivation/reactivation must not delete resource data but preserve it for audit and historical planning purposes.
- Resources must be searchable and filterable by code, description, kind of resource, and status.

**Clarifications:**

- Operational capacity must be type-specific (e.g., containers/hour for cranes, containers/trip for trucks).
- Resources may be assigned to specific areas (e.g., Dock A, Yard B), but this is optional.

---

### 1.3. Acceptance Criteria

- Each resource must include all mandatory fields (code, description, type, capacity, status).
- Qualification requirements must be validated against existing qualifications.
- Setup time must be considered during scheduling.
- Deactivation must preserve historical data.
- Resources must be searchable and filterable by multiple attributes.
- The operation must respect the system’s layered architecture principles.

---

### 1.4. Found Dependencies

- US_2.2.13 – Register Qualifications
- US_2.2.11 – Register Operating Staff
- Authentication / Authorization module

---

### 1.5. Other Relevant Remarks

- The registration process follows the Clean Architecture dependency rule (UI → Application → Domain).
- Resource data must be structured to support intelligent scheduling algorithms and operational planning.

---