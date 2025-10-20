# US_2.2.4 – Register/Update Storage Areas

## 1. Requirements

### 1.1. User Story

**As a Port Authority Officer**,  
I want to register and update storage areas  
so that (un)loading and storage operations can be assigned to the correct locations.

---

### 1.2. Customer Specifications and Clarifications

**From the specification document:**

- “As a Port Authority Officer, I want to register and update storage areas so that (un)loading and storage operations can be assigned to the correct locations.”
- Each storage area must have a unique identifier, type (e.g., yard, warehouse), and location within the port.
- Storage areas must specify maximum capacity (in TEUs) and current occupancy.
- By default, a storage area serves the entire port. However, some yards may be constrained to serve only specific docks.
- Complementary information, such as the distance between docks and storage areas, must be manually recorded to support future logistics planning and optimization.
- Updates must not allow current occupancy to exceed maximum capacity.

**Clarifications:**

- The system must support both general-purpose and dock-specific storage areas.
- Capacity constraints must be enforced during updates and task planning.

---

### 1.3. Acceptance Criteria

- Storage areas must include all mandatory fields (ID, type, location, max capacity, current occupancy).
- The system must prevent updates that exceed maximum capacity.
- It must be possible to associate specific docks with storage areas.
- Distance metadata must be recordable and retrievable.
- The operation must respect the system’s layered architecture principles.

---

### 1.4. Found Dependencies

- US_2.2.3 – Register Docks
- US_2.2.12 – Register Physical Resources
- Authentication / Authorization module

---

### 1.5. Other Relevant Remarks

- The registration process follows the Clean Architecture dependency rule (UI → Application → Domain).
- Data persistence and distance metadata are handled by infrastructure components abstracted through repository and service interfaces.

---

## 2. Analysis

### 2.1. User Story Overview

**ID:** US_2.2.4  
**Title:** Register Storage Areas

**Description:**  
As a Port Authority Officer, I want to register and update storage areas so that (un)loading and storage operations can be assigned to the correct locations.  
Each storage area includes identifiers, type, location, capacity, occupancy, and optional dock associations.

---

### Stakeholders

- **Primary Actor:** Port Authority Officer
- **Other Stakeholders:** Logistics Operators, Port Administration

---

### 2.2. Business Rules

1. **User Authentication:**
    - Only authenticated users with the role *Port Authority Officer* can register storage areas.
2. **Data Consistency:**
    - Each storage area must have a unique identifier.
    - Maximum capacity must be greater than or equal to current occupancy.
    - Dock associations must reference valid dock records.
3. **Distance Metadata:**
    - Manual input of distance between docks and storage areas must be supported.

---

## 3. Design

### 3.1. System Sequence Diagram (SSD)



---

### 3.2. Class Diagram (Conceptual)

**StorageArea**

- id, type, location, maxCapacity, currentOccupancy

**Dock**

- id, name, location

**DistanceMetadata**

- dockId, storageAreaId, distanceInMeters

**Relationship:**  
`StorageArea 1..* — 0..* Dock`  
`DistanceMetadata 1 — 1 StorageArea-Dock pair`

---

### 3.3. Interaction Responsibility Assignment

| Interaction Step                                     | Question: Which class is responsible for…      | Class                          | Justification (with patterns)                                               |
|------------------------------------------------------|------------------------------------------------|--------------------------------|----------------------------------------------------------------------------|
| Step 1: Officer requests to register a storage area  | Handling user input and triggering the process | `RegisterStorageAreaUI`        | **Pure Fabrication:** Manages interaction and delegates to controller.     |
| Step 2: UI forwards the registration request         | Coordinating business logic                    | `RegisterStorageAreaController`| **Controller:** Coordinates actions and delegates to application services. |
| Step 3: Data validation and persistence              | Validating input and executing business rules  | `RegisterStorageAreaService`   | **Information Expert:** Centralizes the business logic.                    |
| Step 4: Persisting data                              | Saving storage area and dock associations      | `IStorageAreaRepository`       | **Repository Pattern:** Handles data persistence logic.                    |
| Step 5: Feedback to user                             | Showing success message                        | `RegisterStorageAreaUI`        | **Pure Fabrication:** Manages user interaction and result display.         |

---

### 3.4. Sequence Diagram



---

### 3.5. SOLID Principles

| Principle                                     | Application in US_2.2.4                                                                              | Explanation                                         |
|----------------------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `RegisterStorageAreaController` handles only coordination of business logic.                         | Keeps UI, logic, and persistence separated.         |
|                                              | `RegisterStorageAreaService` validates and processes business rules.                                 | Focused solely on storage area logic.               |
|                                              | `StorageArea` encapsulates domain logic.                                                             | Isolates business entities from infrastructure.     |
| **O – Open/Closed Principle (OCP)**           | Services and domain classes can be extended with new validations without changing existing logic.    | Allows extensibility with minimal modification.     |
| **L – Liskov Substitution Principle (LSP)**   | `IStorageAreaRepository` can be replaced by any implementation (e.g., SQL, in-memory).               | Promotes interchangeable components.                |
| **I – Interface Segregation Principle (ISP)** | Interfaces like `IStorageAreaRepository` are focused on essential operations (`save`, `findById`).   | Prevents unnecessary dependencies.                  |
| **D – Dependency Inversion Principle (DIP)**  | The controller depends on abstractions (`IStorageAreaRepository`).                                   | Promotes decoupling and enables testing with mocks. |

---

### 3.6. GoF Patterns

| Pattern                          | Usage in US_2.2.4                                                                                     | Explanation                                       |
|----------------------------------|--------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| **Controller**                   | `RegisterStorageAreaController` coordinates the flow between UI, domain, and persistence.             | Acts as mediator between layers.                  |
| **Repository**                   | `IStorageAreaRepository` encapsulates persistence operations.                                          | Abstracts database access.                        |
| **Information Expert**           | `StorageArea` contains its own validation logic.                                                       | Domain entity holds relevant knowledge.           |
| **Pure Fabrication**             | `RegisterStorageAreaUI` exists for technical separation of concerns.                                   | Improves maintainability and testability.         |
| **Low Coupling / High Cohesion**| Clear separation between layers and well-defined responsibilities.                                     | Reduces interdependencies and increases cohesion. |

---

## 4. Tests

### 4.1. Unit Tests

- Validate storage area creation with valid data.
- Verify dock associations and distance metadata.
- Ensure updates do not exceed maximum capacity.
- Confirm persistence of storage area data.

### 4.2. Functional Test

- Register a new storage area via the UI → system confirms creation.
- Attempt to update occupancy beyond capacity → system displays error message.

---

# 5. User Story Implementation Report

---

## 5.1. US_2.2.4 – Register Storage Areas

### 5.2. Description

As a Port Authority Officer, I want to register and update storage areas so that (un)loading and storage operations can be assigned to the correct locations.

### 5.3. Implementation Details

- **User Interaction & Flow Control**  
  A new UI (`RegisterStorageAreaUI`) was developed to allow Port Authority Officers to input storage area details.

- **Business Logic Coordination**  
  The `RegisterStorageAreaController` manages the registration process and delegates validation and persistence to the application service.

- **Domain Entity Construction**  
  The `StorageArea` entity encapsulates storage area data and validation logic.

- **Persistence**  
  The `IStorageAreaRepository` interface and its implementation (`StorageAreaRepositorySQL`) persist storage area data and dock associations in the database.

---
