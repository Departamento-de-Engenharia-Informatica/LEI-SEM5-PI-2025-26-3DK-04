# US_2.2.3 – Register/Update Docks

## 1. Requirements

### 1.1. User Story

**As a Port Authority Officer**,  
I want to register and update docks  
so that the system accurately reflects the docking capacity of the port.

---

### 1.2. Customer Specifications and Clarifications

**From the specification document:**

- “As a Port Authority Officer, I want to register and update docks so that the system accurately reflects the docking capacity of the port.”
- A dock record must include a unique identifier, name/number, location within the port, and physical characteristics (e.g., length, depth, max draft).
- The officer must specify the vessel types allowed to berth there.
- Docks must be searchable and filterable by name, vessel type, and location.

**Clarifications:**

- Location may be represented by coordinates or internal port zones.
- Vessel type associations must support multiple types per dock.

---

### 1.3. Acceptance Criteria

- Each dock must include all mandatory fields (ID, name, location, physical characteristics).
- Multiple vessel types must be assignable to a dock.
- Docks must be searchable and filterable by name, vessel type, and location.
- The operation must respect the system’s layered architecture principles.

---

### 1.4. Found Dependencies

- US_2.2.1 – Register Vessel Types
- Authentication / Authorization module

---

### 1.5. Other Relevant Remarks

- The registration process follows the Clean Architecture dependency rule (UI → Application → Domain).
- Data persistence is handled by infrastructure components abstracted through repository interfaces.

---

## 2. Analysis  

### 2.1. User Story Overview

**ID:** US_2.2.3  
**Title:** Register Docks

**Description:**  
As a Port Authority Officer, I want to register and update docks so that the system accurately reflects the docking capacity of the port.  
Each dock includes identifiers, location, physical characteristics, and allowed vessel types.

---

### Stakeholders

- **Primary Actor:** Port Authority Officer
- **Other Stakeholders:** Port Authority Administration, Shipping Agents

---

### 2.2. Business Rules

1. **User Authentication:**
    - Only authenticated users with the role *Port Authority Officer* can register docks.
2. **Data Consistency:**
    - Each dock must have a unique identifier.
    - Vessel types assigned to a dock must exist in the system.
3. **Searchability:**
    - Docks must be searchable by name, vessel type, and location.

---

## 3. Design

### 3.1. System Sequence Diagram (SSD)

*(To be defined during implementation)*

---

### 3.2. Class Diagram (Conceptual)

**Dock**

- id, name, location, length, depth, maxDraft

**VesselType**

- id, name, description, capacity

**Relationship:**  
`Dock 1..* — 0..* VesselType`

---

### 3.3. Interaction Responsibility Assignment

| Interaction Step                                 | Question: Which class is responsible for…      | Class                         | Justification (with patterns)                                               |
|--------------------------------------------------|------------------------------------------------|-------------------------------|----------------------------------------------------------------------------|
| Step 1: Officer requests to register a dock      | Handling user input and triggering the process | `RegisterDockUI`              | **Pure Fabrication:** Manages interaction and delegates to controller.     |
| Step 2: UI forwards the registration request     | Coordinating business logic                    | `RegisterDockController`      | **Controller:** Coordinates actions and delegates to application services. |
| Step 3: Data validation and persistence          | Validating input and executing business rules  | `RegisterDockService`         | **Information Expert:** Centralizes the business logic.                    |
| Step 4: Persisting data                          | Saving dock and vessel type associations       | `IDockRepository`             | **Repository Pattern:** Handles data persistence logic.                    |
| Step 5: Feedback to user                         | Showing success message                        | `RegisterDockUI`              | **Pure Fabrication:** Manages user interaction and result display.         |

---

### 3.4. Sequence Diagram

*(To be defined during implementation)*

---

### 3.5. SOLID Principles

| Principle                                     | Application in US_2.2.3                                                                              | Explanation                                         |
|----------------------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `RegisterDockController` handles only coordination of business logic.                               | Keeps UI, logic, and persistence separated.         |
|                                              | `RegisterDockService` validates and processes business rules.                                       | Focused solely on dock registration logic.          |
|                                              | `Dock` encapsulates domain logic.                                                                   | Isolates business entities from infrastructure.     |
| **O – Open/Closed Principle (OCP)**           | Services and domain classes can be extended with new validations without changing existing logic.    | Allows extensibility with minimal modification.     |
| **L – Liskov Substitution Principle (LSP)**   | `IDockRepository` can be replaced by any implementation (e.g., SQL, in-memory).                      | Promotes interchangeable components.                |
| **I – Interface Segregation Principle (ISP)** | Interfaces like `IDockRepository` are focused on essential operations (`save`, `findById`).          | Prevents unnecessary dependencies.                  |
| **D – Dependency Inversion Principle (DIP)**  | The controller depends on abstractions (`IDockRepository`).                                          | Promotes decoupling and enables testing with mocks. |

---

### 3.6. GoF Patterns

| Pattern                          | Usage in US_2.2.3                                                                                    | Explanation                                       |
|----------------------------------|------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| **Controller**                   | `RegisterDockController` coordinates the flow between UI, domain, and persistence.                  | Acts as mediator between layers.                  |
| **Repository**                   | `IDockRepository` encapsulates persistence operations.                                               | Abstracts database access.                        |
| **Information Expert**           | `Dock` contains its own validation logic.                                                            | Domain entity holds relevant knowledge.           |
| **Pure Fabrication**             | `RegisterDockUI` exists for technical separation of concerns.                                        | Improves maintainability and testability.         |
| **Low Coupling / High Cohesion**| Clear separation between layers and well-defined responsibilities.                                   | Reduces interdependencies and increases cohesion. |

---

## 4. Tests

### 4.1. Unit Tests

- Validate dock creation with valid data.
- Verify vessel type associations.
- Ensure duplicate dock IDs are rejected.
- Confirm persistence of dock data.

### 4.2. Functional Test

- Register a new dock via the UI → system confirms creation.
- Attempt to register a dock without vessel types → system displays error message.

---

# 5. User Story Implementation Report

---

## 5.1. US_2.2.3 – Register Docks

### 5.2. Description

As a Port Authority Officer, I want to register and update docks so that the system accurately reflects the docking capacity of the port.

### 5.3. Implementation Details

- **User Interaction & Flow Control**  
  A new UI (`RegisterDockUI`) was developed to allow Port Authority Officers to input dock details.

- **Business Logic Coordination**  
  The `RegisterDockController` manages the registration process and delegates validation and persistence to the application service.

- **Domain Entity Construction**  
  The `Dock` entity encapsulates dock data and validation logic.

- **Persistence**  
  The `IDockRepository` interface and its implementation (`DockRepositorySQL`) persist dock data and vessel type associations in the database.

---
