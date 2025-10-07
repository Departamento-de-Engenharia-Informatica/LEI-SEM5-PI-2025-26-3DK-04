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

## 2. Analysis

### 2.1. User Story Overview

**ID:** US_2.2.12  
**Title:** Register Physical Resources

**Description:**  
As a Logistics Operator, I want to register and manage physical resources so that they can be accurately considered during planning and scheduling operations.  
Resources include cranes, trucks, and other equipment, each with specific capacity, setup time, and qualification requirements.

---

### Stakeholders

- **Primary Actor:** Logistics Operator
- **Other Stakeholders:** Port Authority Officer, Operating Staff

---

### 2.2. Business Rules

1. **User Authentication:**
    - Only authenticated users with the role *Logistics Operator* can manage physical resources.
2. **Data Validation:**
    - Resource codes must be unique.
    - Qualification requirements must reference valid qualifications.
    - Setup time must be numeric and non-negative.
3. **Status Management:**
    - Resources can be marked as active, inactive, or under maintenance.
    - Deactivation must retain historical data for audit purposes.

---

## 3. Design

### 3.1. System Sequence Diagram (SSD)

*(To be defined during implementation)*

---

### 3.2. Class Diagram (Conceptual)

**PhysicalResource**

- code, description, type, capacity, status, setupTime, assignedArea

**Qualification**

- code, name

**Relationship:**  
`PhysicalResource 1 — 0..* Qualification`

---

### 3.3. Interaction Responsibility Assignment

| Interaction Step                                     | Question: Which class is responsible for…      | Class                             | Justification (with patterns)                                               |
|------------------------------------------------------|------------------------------------------------|-----------------------------------|----------------------------------------------------------------------------|
| Step 1: Operator initiates resource registration     | Handling user input and triggering the process | `RegisterResourceUI`              | **Pure Fabrication:** Manages interaction and delegates to controller.     |
| Step 2: UI forwards the registration request         | Coordinating business logic                    | `RegisterResourceController`      | **Controller:** Coordinates actions and delegates to application services. |
| Step 3: Data validation and persistence              | Validating input and executing business rules  | `RegisterResourceService`         | **Information Expert:** Centralizes the business logic.                    |
| Step 4: Persisting data                              | Saving resource and qualification links        | `IResourceRepository`             | **Repository Pattern:** Handles data persistence logic.                    |
| Step 5: Feedback to user                             | Showing success message                        | `RegisterResourceUI`              | **Pure Fabrication:** Manages user interaction and result display.         |

---

### 3.4. Sequence Diagram

*(To be defined during implementation)*

---

### 3.5. SOLID Principles

| Principle                                     | Application in US_2.2.12                                                                             | Explanation                                         |
|----------------------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `RegisterResourceController` handles only coordination of business logic.                           | Keeps UI, logic, and persistence separated.         |
|                                              | `RegisterResourceService` validates and processes business rules.                                   | Focused solely on resource logic.                   |
|                                              | `PhysicalResource` encapsulates domain logic.                                                       | Isolates business entities from infrastructure.     |
| **O – Open/Closed Principle (OCP)**           | Services and domain classes can be extended with new validations without changing existing logic.    | Allows extensibility with minimal modification.     |
| **L – Liskov Substitution Principle (LSP)**   | `IResourceRepository` can be replaced by any implementation (e.g., SQL, in-memory).                  | Promotes interchangeable components.                |
| **I – Interface Segregation Principle (ISP)** | Interfaces like `IResourceRepository` are focused on essential operations (`save`, `findById`).      | Prevents unnecessary dependencies.                  |
| **D – Dependency Inversion Principle (DIP)**  | The controller depends on abstractions (`IResourceRepository`).                                      | Promotes decoupling and enables testing with mocks. |

---

### 3.6. GoF Patterns

| Pattern                          | Usage in US_2.2.12                                                                                    | Explanation                                       |
|----------------------------------|--------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| **Controller**                   | `RegisterResourceController` coordinates the flow between UI, domain, and persistence.                | Acts as mediator between layers.                  |
| **Repository**                   | `IResourceRepository` encapsulates persistence operations.                                             | Abstracts database access.                        |
| **Information Expert**           | `PhysicalResource` contains its own validation logic.                                                  | Domain entity holds relevant knowledge.           |
| **Pure Fabrication**             | `RegisterResourceUI` exists for technical separation of concerns.                                      | Improves maintainability and testability.         |
| **Low Coupling / High Cohesion**| Clear separation between layers and well-defined responsibilities.                                     | Reduces interdependencies and increases cohesion. |

---

## 4. Tests

### 4.1. Unit Tests

- Validate resource creation with valid data.
- Verify qualification requirements.
- Ensure setup time is correctly stored and used.
- Confirm status transitions and historical data retention.

### 4.2. Functional Test

- Register a new physical resource via the UI → system confirms creation.
- Attempt to register with duplicate code → system displays error message.
- Deactivate a resource → system preserves data and marks status correctly.

---

# 5. User Story Implementation Report

---

## 5.1. US_2.2.12 – Register Physical Resources

### 5.2. Description

As a Logistics Operator, I want to register and manage physical resources so that they can be accurately considered during planning and scheduling operations.

### 5.3. Implementation Details

- **User Interaction & Flow Control**  
  A new UI (`RegisterResourceUI`) was developed to allow Logistics Operators to input resource details.

- **Business Logic Coordination**  
  The `RegisterResourceController` manages the registration process and delegates validation and persistence to the application service.

- **Domain Entity Construction**  
  The `PhysicalResource` entity encapsulates resource data, operational capacity, setup time, and qualification requirements.

- **Persistence**  
  The `IResourceRepository` interface and its implementation (`ResourceRepositorySQL`) persist resource data and associations in the database.

---