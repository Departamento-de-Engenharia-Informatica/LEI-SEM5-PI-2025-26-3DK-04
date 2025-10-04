# US_2.2.5 – Register Shipping Agent Organizations

## 1. Requirements

### 1.1. User Story

**As a Port Authority Officer**,  
I want to register new shipping agent organizations  
so that they can operate within the port’s digital system.

---

### 1.2. Customer Specifications and Clarifications

**From the specification document:**

- “As a Port Authority Officer, I want to register new shipping agent organizations so that they can operate within the
  port’s digital system.”
- Each organization must include at least an identifier, legal and alternative names, an address, and a tax number.
- Each organization must include at least one representative at the time of registration.
- Representatives must be registered with name, citizen ID, nationality, email, and phone number.
- Email and phone number are used for system notifications, including approval decisions and authentication.

**Clarifications:**



---

### 1.3. Acceptance Criteria

- Each organization must contain all mandatory fields (ID, name, address, tax number).
- At least one representative must be added at the time of registration.
- Representatives must include valid contact information (email, phone).
- A confirmation email must be sent to the registered representative.
- The operation must respect the system’s layered architecture principles.

---

### 1.4. Found Dependencies

- US_2.2.6 – Edit Shipping Agent
- US_2.2.7 – Approve Shipping Agent
- Authentication / Authorization module

---

### 1.5. Other Relevant Remarks

- The registration process follows the dependency rule defined in the Clean Architecture approach (UI → Application →
  Domain).
- Data persistence and notifications are handled by infrastructure components, abstracted through repository and service
  interfaces.

---

## 2. Analysis

### 2.1. User Story Overview

**ID:** US_2.2.5  
**Title:** Register Shipping Agent Organizations

**Description:**  
As a Port Authority Officer, I want to register new shipping agent organizations so that they can operate within the
port’s digital system.  
Each organization includes identifiers, names, address, tax number, and representatives responsible for communication
and authentication.

---

### Stakeholders

- **Primary Actor:** Port Authority Officer
- **Other Stakeholders:** Port Authority Administration, Shipping Agents

---

### 2.2. Business Rules

1. **User Authentication:**
    - Only authenticated users with the role *Port Authority Officer* can register shipping agents.
2. **Data Consistency:**
    - Each organization must include at least one representative.
    - Each representative must have a unique email and citizen ID.
    - Organization tax numbers must be unique in the system.
3. **Notification:**
    - Upon successful registration, an automatic email must be sent to all representatives.

---

## 3. Design

### 3.1. System Sequence Diagram (SSD)



---

### 3.2. Class Diagram (Conceptual)

**ShippingAgentOrganization**

- id, legalName, altName, address, taxNumber

**Representative**

- name, citizenId, nationality, email, phone

**Relationship:**  
`ShippingAgentOrganization 1..* — 1..* Representative`

---

### 3.3. Interaction Responsibility Assignment

| Interaction Step                                        | Question: Which class is responsible for…      | Class                             | Justification (with patterns)                                              |
|---------------------------------------------------------|------------------------------------------------|-----------------------------------|----------------------------------------------------------------------------|
| Step 1: Officer requests to register a new organization | Handling user input and triggering the process | `RegisterShippingAgentUI`         | **Pure Fabrication:** Handles interaction and delegates to controller.     |
| Step 2: UI forwards the registration request            | Coordinating business logic                    | `RegisterShippingAgentController` | **Controller:** Coordinates actions and delegates to application services. |
| Step 3: Data validation and persistence                 | Validating input and executing business rules  | `RegisterShippingAgentService`    | **Information Expert:** Centralizes the business logic.                    |
| Step 4: Persisting data                                 | Saving organization and representatives        | `IShippingAgentRepository`        | **Repository Pattern:** Handles data persistence logic.                    |
| Step 5: Confirmation email                              | Sending notification to representatives        | `EmailNotificationService`        | **Pure Fabrication:** Handles external notification.                       |
| Step 6: Feedback to user                                | Showing success message                        | `RegisterShippingAgentUI`         | **Pure Fabrication:** Manages user interaction and result display.         |

---

### 3.4. Sequence Diagram



### 3.5. SOLID Principles

| Principle                                     | Application in US_2.2.5                                                                              | Explanation                                         |
|-----------------------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `RegisterShippingAgentController` handles only coordination of business logic.                       | Keeps UI, logic, and persistence separated.         |
|                                               | `RegisterShippingAgentService` validates and processes business rules.                               | Focused solely on registration logic.               |
|                                               | `ShippingAgentOrganization` and `Representative` encapsulate domain logic.                           | Isolate business entities from infrastructure.      |
| **O – Open/Closed Principle (OCP)**           | Services and domain classes can be extended with new validations without changing existing logic.    | Allows extensibility with minimal modification.     |
| **L – Liskov Substitution Principle (LSP)**   | `IShippingAgentRepository` can be replaced by any implementation (e.g., SQL, in-memory).             | Promotes interchangeable components.                |
| **I – Interface Segregation Principle (ISP)** | Interfaces like `IShippingAgentRepository` are focused on essential operations (`save`, `findById`). | Prevents unnecessary dependencies.                  |
| **D – Dependency Inversion Principle (DIP)**  | The controller depends on abstractions (`IShippingAgentRepository`, `EmailNotificationService`).     | Promotes decoupling and enables testing with mocks. |

---

### 3.6. GoF Patterns

| Pattern                          | Usage in US_2.2.5                                                                                    | Explanation                                       |
|----------------------------------|------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| **Controller**                   | `RegisterShippingAgentController` coordinates the flow between UI, domain, and persistence.          | Acts as mediator between layers.                  |
| **Repository**                   | `IShippingAgentRepository` encapsulates persistence operations.                                      | Abstracts database access.                        |
| **Information Expert**           | `ShippingAgentOrganization` and `Representative` contain their own validation logic.                 | Domain entities hold relevant knowledge.          |
| **Pure Fabrication**             | `RegisterShippingAgentUI` and `EmailNotificationService` exist for technical separation of concerns. | Improves maintainability and testability.         |
| **Low Coupling / High Cohesion** | Clear separation between layers and well-defined responsibilities.                                   | Reduces interdependencies and increases cohesion. |

---

## 4. Tests

### 4.1. Unit Tests

- Validate organization creation with valid data.
- Verify at least one representative is included.
- Ensure duplicate tax numbers or emails are rejected.
- Check automatic email notification is triggered.

### 4.2. Functional Test

- Register a new organization via the UI → system confirms creation and sends email notification.
- Attempt to register organization without representative → system displays error message.

---

# 5. User Story Implementation Report

---

## 5.1. US_2.2.5 – Register Shipping Agent Organizations

### 5.2. Description

As a Port Authority Officer, I want to register new shipping agent organizations so that they can operate within the
port’s digital system.

### 5.3. Implementation Details

- **User Interaction & Flow Control**  
  A new UI (`RegisterShippingAgentUI`) was developed to allow Port Authority Officers to input organization and
  representative details.

- **Business Logic Coordination**  
  The `RegisterShippingAgentController` manages the registration process and delegates validation and persistence to the
  application service.

- **Domain Entity Construction**  
  The `ShippingAgentOrganization` entity encapsulates organization data and validation logic, while `Representative`
  stores contact and identification details.

- **Persistence**  
  The `IShippingAgentRepository` interface and its implementation (`ShippingAgentRepositorySQL`) persist organization
  and representative data in the database.

- **Notifications**  
  The `EmailNotificationService` sends confirmation messages to representatives after successful registration.

---
