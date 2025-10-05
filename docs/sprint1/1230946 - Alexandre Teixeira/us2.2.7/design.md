## 3. Design

### 3.1. System Sequence Diagram (SSD)



---

### 3.2. Class Diagram 



---

### 3.3. Interaction Responsibility Assignment

| Interaction Step                                                  | Question: Which class is responsible for…      | Class                                    | Justification (with patterns)                                              |
|-------------------------------------------------------------------|------------------------------------------------|------------------------------------------|----------------------------------------------------------------------------|
| Step 1: Officer requests to review pending notifications          | Handling user input and triggering the process | `ReviewVesselVisitNotificationUI`        | **Pure Fabrication:** Handles interaction and delegates to controller.     |
| Step 2: Retrieve pending notifications                            | Fetching pending notifications                 | `ReviewVesselVisitNotificationController`| **Controller:** Coordinates actions and delegates to application services. |
| Step 3: Officer selects a notification and makes decision         | Displaying data and capturing user input       | `ReviewVesselVisitNotificationUI`        | **Pure Fabrication:** Manages user interaction.                            |
| Step 4: Process approval/rejection with business rules            | Validating decision and executing business rules| `VesselVisitNotificationService`        | **Information Expert:** Centralizes the business logic.                    |
| Step 5: Assign dock (if approved) or record rejection reason      | Managing notification status and dock assignment| `VesselVisitNotification`               | **Information Expert:** Domain entity holds relevant knowledge.            |
| Step 6: Log decision with timestamp, officer ID, and outcome      | Recording audit trail                          | `DecisionAuditLog`                       | **Information Expert:** Encapsulates audit logging logic.                  |
| Step 7: Persist updated notification and audit log                | Saving notification and audit data             | `VesselVisitNotificationRepository`     | **Repository Pattern:** Handles data persistence logic.                    |
| Step 8: Notify shipping agent (if rejected)                       | Sending notification to shipping agent         | `NotificationService`                    | **Pure Fabrication:** Handles external notification.                       |
| Step 9: Feedback to officer                                       | Showing success/error message                  | `ReviewVesselVisitNotificationUI`        | **Pure Fabrication:** Manages user interaction and result display.         |

---

### 3.4. Sequence Diagram



### 3.5. SOLID Principles

| Principle                                     | Application in US_2.2.7                                                                                          | Explanation                                         |
|-----------------------------------------------|------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `ReviewVesselVisitNotificationController` handles only coordination of business logic.                           | Keeps UI, logic, and persistence separated.         |
|                                               | `VesselVisitNotificationService` validates and processes business rules for approval/rejection.                  | Focused solely on notification review logic.        |
|                                               | `VesselVisitNotification` encapsulates domain logic for notification status and dock assignment.                 | Isolate business entities from infrastructure.      |
|                                               | `DecisionAuditLog` handles audit trail recording with timestamp, officer ID, and decision outcome.               | Separates auditing concerns from core logic.        |
| **O – Open/Closed Principle (OCP)**           | Services and domain classes can be extended with new decision types or validation rules without changing logic.  | Allows extensibility with minimal modification.     |
| **L – Liskov Substitution Principle (LSP)**   | `VesselVisitNotificationRepository` can be replaced by any implementation (e.g., SQL, in-memory, NoSQL).        | Promotes interchangeable components.                |
| **I – Interface Segregation Principle (ISP)** | Interfaces like `VesselVisitNotificationRepository` are focused on essential operations (`findPending`, `save`).| Prevents unnecessary dependencies.                  |
| **D – Dependency Inversion Principle (DIP)**  | The controller depends on abstractions (`VesselVisitNotificationRepository`, `NotificationService`).            | Promotes decoupling and enables testing with mocks. |

---

### 3.6. GoF Patterns

| Pattern                          | Usage in US_2.2.7                                                                                                | Explanation                                       |
|----------------------------------|------------------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| **Controller**                   | `ReviewVesselVisitNotificationController` coordinates the flow between UI, domain, and persistence.              | Acts as mediator between layers.                  |
| **Repository**                   | `VesselVisitNotificationRepository` encapsulates persistence operations for notifications and audit logs.       | Abstracts database access.                        |
| **Information Expert**           | `VesselVisitNotification` contains its own validation logic for approval/rejection and dock assignment.          | Domain entities hold relevant knowledge.          |
|                                  | `DecisionAuditLog` manages audit trail data with timestamp, officer ID, and decision outcome.                    | Encapsulates audit-specific information.          |
| **Pure Fabrication**             | `ReviewVesselVisitNotificationUI` and `NotificationService` exist for technical separation of concerns.          | Improves maintainability and testability.         |
| **State Pattern** (optional)     | `VesselVisitNotification` status transitions (Pending → Approved/Rejected) can be modeled using state pattern.   | Manages different notification states cleanly.    |
| **Low Coupling / High Cohesion** | Clear separation between layers and well-defined responsibilities.                                               | Reduces interdependencies and increases cohesion. |

---