# US_2.2.9 – Change or Complete a Vessel Visit Notification

## 1. Requirements

### 1.1. User Story

**As a Shipping Agent Representative**,  
I want to change or complete a Vessel Visit Notification while it is still in progress,  
so that I can correct errors or withdraw requests if necessary.

---

### 1.2. Customer Specifications and Clarifications

**From the specification document:**

- “As a Shipping Agent Representative, I want to change / complete a Vessel Visit Notification while it is still in
  progress, so that I can correct errors or withdraw requests if necessary.”
- Status can be maintained as “in progress” or changed to “submitted / approval pending” by the representative.

**Clarifications:**


---

### 1.3. Acceptance Criteria

- Representatives can modify details while the status is “in progress.”
- When the representative submits the notification, the status changes to “submitted / approval pending.”
- Once submitted, the notification cannot be edited but can be withdrawn.
- All modifications must be recorded with timestamps and user identification.

---

### 1.4. Found Dependencies

- US_2.2.8 – Create Vessel Visit Notification
- Authentication / Authorization module


---

### 1.5. Other Relevant Remarks

- The notification’s lifecycle is managed by status transitions: **In Progress → Submitted / Approval Pending →
  Approved / Rejected**.
- Must comply with the dependency inversion and separation of concerns principles described in *Layered Architecture
  Patterns v8*.

---

## 2. Analysis

### 2.1. User Story Overview

**ID:** US_2.2.9  
**Title:** Change or Complete a Vessel Visit Notification

**Description:**  
As a Shipping Agent Representative, I want to modify or complete a Vessel Visit Notification while it is still in
progress, allowing correction of errors or withdrawal before submission.

---

### Stakeholders

- **Primary Actor:** Shipping Agent Representative
- **Other Stakeholders:** Port Authority Officers, Vessel Management System

---

### 2.2. Business Rules

1. **Status Control:**
    - Notifications in “in progress” status can be edited.
    - Notifications marked as “submitted” become read-only.
2. **Data Integrity:**
    - All changes must be logged with timestamp and user ID.
3. **Withdrawal Rule:**
    - Representatives can withdraw notifications prior to approval.
4. **Validation:**
    - Submission triggers validation of all required fields.

---

## 3. Design

### 3.1. System Sequence Diagram (SSD)


---

### 3.2. Class Diagram (Conceptual)

**VesselVisitNotification**

- id, vesselName, eta, etd, status, lastModified, submittedBy

**Representative**

- id, name, email

**Relationship:**  
`Representative 1 — * VesselVisitNotification`

---

### 3.3. SOLID Principles

| Principle | Application                                                                           | Explanation                               |
|-----------|---------------------------------------------------------------------------------------|-------------------------------------------|
| **SRP**   | `ManageVesselVisitController` focuses on coordination.                                | UI, logic, and persistence are separated. |
| **OCP**   | New notification states or validations can be added without changing existing code.   | Extensible design.                        |
| **LSP**   | `IVesselVisitRepository` can be substituted by any implementation.                    | Enables flexible persistence.             |
| **ISP**   | Repositories and services expose minimal required operations.                         | Prevents unnecessary coupling.            |
| **DIP**   | High-level modules depend on abstractions (`IVesselVisitRepository`, `AuditService`). | Promotes testability and modularity.      |

---

### 3.4. GoF Patterns

| Pattern              | Usage                                                            | Explanation                                     |
|----------------------|------------------------------------------------------------------|-------------------------------------------------|
| **Controller**       | `ManageVesselVisitController` coordinates UI and services.       | Mediates between presentation and domain.       |
| **Repository**       | `IVesselVisitRepository` handles persistence operations.         | Abstracts database access.                      |
| **State**            | The notification status transitions follow a finite state model. | Controls allowed operations based on state.     |
| **Observer / Event** | `AuditService` reacts to modifications.                          | Ensures change tracking without tight coupling. |

---

## 4. Tests

### 4.1. Unit Tests

- Edit notification while in progress → success.
- Try editing after submission → rejected.
- Submit valid notification → status changes to “submitted.”
- Withdraw in-progress notification → marked as withdrawn.

### 4.2. Functional Test

- Representative edits and submits a notification via UI → system saves, logs, and confirms status transition.
- Representative withdraws notification → system updates status and logs the event.

---

# 5. User Story Implementation Report

## 5.1. US_2.2.9 – Change or Complete a Vessel Visit Notification

### 5.2. Description

As a Shipping Agent Representative, I want to edit or complete a Vessel Visit Notification while it’s still in progress,
so I can correct mistakes or withdraw requests when necessary.

### 5.3. Implementation Details

- **User Interaction & Flow Control:**  
  `ManageVesselVisitUI` allows editing, submission, and withdrawal.
- **Business Logic:**  
  `ManageVesselVisitService` enforces state rules and handles validations.
- **Persistence:**  
  `IVesselVisitRepository` updates and retrieves notification data.
- **Audit Logging:**  
  `AuditService` records all modifications, including timestamps and user identifiers.

---
