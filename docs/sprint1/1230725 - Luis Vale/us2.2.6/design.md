## 3. Design

### 3.1. System Sequence Diagram (SSD)

---

### 3.2. Class Diagram (Conceptual)

---

### 3.3. SOLID Principles

| Principle                                     | Application                                                                                       | Explanation                                 |
|-----------------------------------------------|---------------------------------------------------------------------------------------------------|---------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `ManageRepresentativeController` coordinates logic only.                                          | Separation of UI, service, and persistence. |
| **O – Open/Closed Principle (OCP)**           | Validation rules and representative operations can be extended without modifying existing code.   | Extensible architecture.                    |
| **L – Liskov Substitution Principle (LSP)**   | Any implementation of `IRepresentativeRepository` can be used.                                    | Supports polymorphism.                      |
| **I – Interface Segregation Principle (ISP)** | Repositories expose only minimal methods (`create`, `update`, `deactivate`).                      | Prevents unnecessary dependencies.          |
| **D – Dependency Inversion Principle (DIP)**  | The controller depends on abstractions (`IRepresentativeRepository`, `EmailNotificationService`). | Enables loose coupling.                     |

---

### 3.4. GoF Patterns

| Pattern                | Usage                                                     | Explanation                              |
|------------------------|-----------------------------------------------------------|------------------------------------------|
| **Controller**         | `ManageRepresentativeController` orchestrates operations. | Coordinates flow between layers.         |
| **Repository**         | `IRepresentativeRepository` abstracts data persistence.   | Manages access to the persistence layer. |
| **Information Expert** | `Representative` contains its validation and state logic. | Encapsulates its own data and behavior.  |
| **Observer / Event**   | Notification service triggers emails upon changes.        | Decouples notification logic.            |

---
