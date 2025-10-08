# US_2.2.8 – Submit Vessel Visit Notification

## 1. Requirements

### 1.1. User Story

**As a Shipping Agent Representative**,  
I want to create and submit a Vessel Visit Notification  
so that the vessel berthing and subsequent (un)loading operations at the port are scheduled and planned in space and timely manner.

---

### 1.2. Customer Specifications and Clarifications

**From the specification document:**

- “As a Shipping Agent Representative, I want to create/submit a Vessel Visit Notification so that the vessel berthing and subsequent (un)loading operations at the port are scheduled and planned in space and timely manner.”
- The Cargo Manifest data for unloading and/or loading is included.
- The system must validate that referred container identifiers comply with the ISO 6346:2022 standard.
- Crew information (name, citizen ID, nationality) may be requested when necessary for compliance with security protocols.
- Vessel Visit Notifications may remain in an "in progress" status until completed.
- Once completed, the representative must change the status to "submitted" to request approval.

**Clarifications:**

- A Vessel Visit may include one, both, or no manifests depending on the nature of the visit.
- Dangerous cargo requires identification of crew safety officers.

---

### 1.3. Acceptance Criteria

- The notification must include vessel ID, ETA, ETD, cargo manifest(s), and crew details if required.
- Container IDs must be validated against ISO 6346:2022.
- The notification must support both loading and unloading manifests.
- The initial status is "in progress" and must be explicitly changed to "submitted" by the representative.
- The operation must respect the system’s layered architecture principles.

---

### 1.4. Found Dependencies

- US_2.2.9 – Update Vessel Visit Notification
- US_2.2.10 – View Vessel Visit Status
- US_2.2.7 – Approve Vessel Visit Notification
- US_2.2.2 – Register Vessel
- US_2.2.6 – Manage Shipping Agent Representatives

---

### 1.5. Other Relevant Remarks

- The submission process follows the Clean Architecture dependency rule (UI → Application → Domain).
- Cargo manifest validation and crew data handling must comply with GDPR and port safety regulations.

---

## 2. Analysis

### 2.1. User Story Overview

**ID:** US_2.2.8  
**Title:** Submit Vessel Visit Notification

**Description:**  
As a Shipping Agent Representative, I want to create and submit a Vessel Visit Notification so that the vessel berthing and subsequent (un)loading operations at the port are scheduled and planned.  
The notification includes vessel details, cargo manifests, and crew information when applicable.

---

### Stakeholders

- **Primary Actor:** Shipping Agent Representative
- **Other Stakeholders:** Port Authority Officer, Logistics Operator

---

### 2.2. Business Rules

1. **User Authentication:**
    - Only authenticated users with the role *Shipping Agent Representative* can submit notifications.
2. **Data Validation:**
    - Container IDs must comply with ISO 6346:2022.
    - Crew safety officers must be identified for hazardous cargo.
3. **Status Management:**
    - Notification starts as "in progress" and must be changed to "submitted" to trigger approval workflow.

---

## 3. Design

### 3.1. System Sequence Diagram (SSD)

*(To be defined during implementation)*

---

### 3.2. Class Diagram (Conceptual)

**VesselVisitNotification**

- id, vesselId, ETA, ETD, status, cargoManifest, crewInfo

**CargoManifest**

- type (loading/unloading), containerList

**Container**

- id (ISO 6346), description, bay, row, tier

**CrewMember**

- name, citizenId, nationality, role

**Relationship:**  
`VesselVisitNotification 1 — 0..* CargoManifest`  
`VesselVisitNotification 1 — 0..* CrewMember`

---

### 3.3. Interaction Responsibility Assignment

| Interaction Step                                     | Question: Which class is responsible for…      | Class                             | Justification (with patterns)                                               |
|------------------------------------------------------|------------------------------------------------|-----------------------------------|----------------------------------------------------------------------------|
| Step 1: Agent initiates notification                 | Handling user input and triggering the process | `SubmitVesselVisitUI`             | **Pure Fabrication:** Manages interaction and delegates to controller.     |
| Step 2: UI forwards the submission request           | Coordinating business logic                    | `SubmitVesselVisitController`     | **Controller:** Coordinates actions and delegates to application services. |
| Step 3: Data validation and persistence              | Validating input and executing business rules  | `SubmitVesselVisitService`        | **Information Expert:** Centralizes the business logic.                    |
| Step 4: Persisting data                              | Saving notification and cargo/crew details     | `IVesselVisitRepository`          | **Repository Pattern:** Handles data persistence logic.                    |
| Step 5: Feedback to user                             | Showing success message                        | `SubmitVesselVisitUI`             | **Pure Fabrication:** Manages user interaction and result display.         |

---

### 3.4. Sequence Diagram

*(To be defined during implementation)*

---

### 3.5. SOLID Principles

| Principle                                     | Application in US_2.2.8                                                                              | Explanation                                         |
|----------------------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `SubmitVesselVisitController` handles only coordination of business logic.                          | Keeps UI, logic, and persistence separated.         |
|                                              | `SubmitVesselVisitService` validates and processes business rules.                                  | Focused solely on notification logic.               |
|                                              | `VesselVisitNotification` encapsulates domain logic.                                                 | Isolates business entities from infrastructure.     |
| **O – Open/Closed Principle (OCP)**           | Services and domain classes can be extended with new validations without changing existing logic.    | Allows extensibility with minimal modification.     |
| **L – Liskov Substitution Principle (LSP)**   | `IVesselVisitRepository` can be replaced by any implementation (e.g., SQL, in-memory).               | Promotes interchangeable components.                |
| **I – Interface Segregation Principle (ISP)** | Interfaces like `IVesselVisitRepository` are focused on essential operations (`save`, `findById`).   | Prevents unnecessary dependencies.                  |
| **D – Dependency Inversion Principle (DIP)**  | The controller depends on abstractions (`IVesselVisitRepository`).                                   | Promotes decoupling and enables testing with mocks. |

---

### 3.6. GoF Patterns

| Pattern                          | Usage in US_2.2.8                                                                                     | Explanation                                       |
|----------------------------------|--------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| **Controller**                   | `SubmitVesselVisitController` coordinates the flow between UI, domain, and persistence.               | Acts as mediator between layers.                  |
| **Repository**                   | `IVesselVisitRepository` encapsulates persistence operations.                                          | Abstracts database access.                        |
| **Information Expert**           | `VesselVisitNotification`, `CargoManifest`, and `Container` contain their own validation logic.       | Domain entities hold relevant knowledge.          |
| **Pure Fabrication**             | `SubmitVesselVisitUI` exists for technical separation of concerns.                                     | Improves maintainability and testability.         |
| **Low Coupling / High Cohesion**| Clear separation between layers and well-defined responsibilities.                                     | Reduces interdependencies and increases cohesion. |

---

## 4. Tests

### 4.1. Unit Tests

- Validate notification creation with valid vessel and cargo data.
- Verify ISO 6346 container ID format.
- Ensure hazardous cargo triggers crew safety officer requirement.
- Confirm status transitions from "in progress" to "submitted".

### 4.2. Functional Test

- Submit a complete Vessel Visit Notification via the UI → system confirms submission.
- Attempt to submit with invalid container ID → system displays error message.
- Submit hazardous cargo without safety officer → system blocks submission.

---

# 5. User Story Implementation Report

---

## 5.1. US_2.2.8 – Submit Vessel Visit Notification

### 5.2. Description

As a Shipping Agent Representative, I want to create and submit a Vessel Visit Notification so that the vessel berthing and subsequent (un)loading operations at the port are scheduled and planned.

### 5.3. Implementation Details

- **User Interaction & Flow Control**  
  A new UI (`SubmitVesselVisitUI`) was developed to allow Shipping Agent Representatives to input vessel, cargo, and crew details.

- **Business Logic Coordination**  
  The `SubmitVesselVisitController` manages the submission process and delegates validation and persistence to the application service.

- **Domain Entity Construction**  
  The `VesselVisitNotification` entity encapsulates visit data, cargo manifests, and crew information.

- **Persistence**  
  The `IVesselVisitRepository` interface and its implementation (`VesselVisitRepositorySQL`) persist notification data in the database.

---