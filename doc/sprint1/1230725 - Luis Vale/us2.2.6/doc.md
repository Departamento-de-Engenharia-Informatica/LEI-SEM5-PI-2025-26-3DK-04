# US_2.2.6 – Register and Manage Representatives of a Shipping Agent Organization

## 1. Requirements

### 1.1. User Story

**As a Port Authority Officer**,  
I want to register and manage representatives of a shipping agent organization (create, update, deactivate),  
so that the right individuals are authorized to interact with the system on behalf of their organization.

---

### 1.2. Customer Specifications and Clarifications

**From the specification document:**

- “As a Port Authority Officer, I want to register and manage representatives of a shipping agent organization (create,
  update, deactivate), so that the right individuals are authorized to interact with the system on behalf of their
  organization.”
- Each representative must be associated with exactly one shipping agent organization.
- Required representative details include: name, citizen ID, nationality, email, and phone number.

**Clarifications:**


---

### 1.3. Acceptance Criteria

- Each representative must belong to one and only one organization.
- The system must allow the creation, update, and deactivation of representatives.
- Duplicate emails or citizen IDs must not be allowed.
- Deactivation must retain the representative’s history and audit data.
- The system must send notifications upon creation or deactivation.

---

### 1.4. Found Dependencies

- US_2.2.5 – Register Shipping Agent Organization
- Authentication / Authorization module

---

### 1.5. Other Relevant Remarks

- The management of representatives follows the same dependency flow (UI → Application → Domain → Infrastructure).
- The functionality must comply with the layered architecture principles defined in *Something About Layered
  Architecture Patterns v8*.

---

## 2. Analysis

### 2.1. User Story Overview

**ID:** US_2.2.6  
**Title:** Register and Manage Representatives of a Shipping Agent Organization

**Description:**  
As a Port Authority Officer, I want to create, update, and deactivate representatives of shipping agent organizations,
ensuring that only authorized individuals can act on behalf of their company within the system.

---

### Stakeholders

- **Primary Actor:** Port Authority Officer
- **Other Stakeholders:** Shipping Agents, Port Authority Administration

---

### 2.2. Business Rules

1. **Association Rule:**
    - A representative must be linked to exactly one organization.
2. **Data Validation:**
    - Required fields: name, citizen ID, nationality, email, phone.
    - Citizen ID and email must be unique.
3. **Deactivation Rule:**
    - Deactivated representatives remain in the system but cannot authenticate.
4. **Notification Rule:**
    - Representatives and their organizations are notified upon creation, update, or deactivation.

---

## 3. Design

### 3.1. System Sequence Diagram (SSD)


---

### 3.2. Class Diagram (Conceptual)

**Representative**

- name, citizenId, nationality, email, phone, status (Active/Inactive)

**ShippingAgentOrganization**

- id, name, taxNumber

**Relationship:**  
`ShippingAgentOrganization 1 — * Representative`

---

### 3.3. SOLID Principles

| Principle | Application                                                                                       | Explanation                                 |
|-----------|---------------------------------------------------------------------------------------------------|---------------------------------------------|
| **SRP**   | `ManageRepresentativeController` coordinates logic only.                                          | Separation of UI, service, and persistence. |
| **OCP**   | Validation rules and representative operations can be extended without modifying existing code.   | Extensible architecture.                    |
| **LSP**   | Any implementation of `IRepresentativeRepository` can be used.                                    | Supports polymorphism.                      |
| **ISP**   | Repositories expose only minimal methods (`create`, `update`, `deactivate`).                      | Prevents unnecessary dependencies.          |
| **DIP**   | The controller depends on abstractions (`IRepresentativeRepository`, `EmailNotificationService`). | Enables loose coupling.                     |

---

### 3.4. GoF Patterns

| Pattern                | Usage                                                     | Explanation                              |
|------------------------|-----------------------------------------------------------|------------------------------------------|
| **Controller**         | `ManageRepresentativeController` orchestrates operations. | Coordinates flow between layers.         |
| **Repository**         | `IRepresentativeRepository` abstracts data persistence.   | Manages access to the persistence layer. |
| **Information Expert** | `Representative` contains its validation and state logic. | Encapsulates its own data and behavior.  |
| **Observer / Event**   | Notification service triggers emails upon changes.        | Decouples notification logic.            |

---

## 4. Tests

### 4.1. Unit Tests

- Create a representative with valid data.
- Attempt to create representative with duplicate email → expect failure.
- Deactivate representative → verify status = Inactive.
- Update contact info and verify persistence.

### 4.2. Functional Test

- Officer creates representative → system confirms creation and sends email.
- Officer deactivates representative → system confirms and blocks access.

---

# 5. User Story Implementation Report

## 5.1. US_2.2.6 – Register and Manage Representatives

### 5.2. Description

As a Port Authority Officer, I want to manage representatives of shipping agent organizations (create, update,
deactivate) so that only authorized users can operate on behalf of their organizations.

### 5.3. Implementation Details

- **User Interaction & Flow Control:**  
  `ManageRepresentativeUI` provides forms for CRUD operations.
- **Business Logic:**  
  `ManageRepresentativeService` validates data and coordinates repository and notification services.
- **Persistence:**  
  `IRepresentativeRepository` persists representative data; inactive users are not removed but flagged.
- **Notifications:**  
  `EmailNotificationService` informs representatives and their organizations about updates.

---
