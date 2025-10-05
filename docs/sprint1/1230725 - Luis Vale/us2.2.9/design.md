## 3. Design

### 3.1. System Sequence Diagram (SSD)

---

### 3.2. Class Diagram (Conceptual)

---

### 3.3. SOLID Principles

| Principle                                     | Application                                                                           | Explanation                               |
|-----------------------------------------------|---------------------------------------------------------------------------------------|-------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `ManageVesselVisitController` focuses on coordination.                                | UI, logic, and persistence are separated. |
| **O – Open/Closed Principle (OCP)**           | New notification states or validations can be added without changing existing code.   | Extensible design.                        |
| **L – Liskov Substitution Principle (LSP)**   | `IVesselVisitRepository` can be substituted by any implementation.                    | Enables flexible persistence.             |
| **I – Interface Segregation Principle (ISP)** | Repositories and services expose minimal required operations.                         | Prevents unnecessary coupling.            |
| **D – Dependency Inversion Principle (DIP)**  | High-level modules depend on abstractions (`IVesselVisitRepository`, `AuditService`). | Promotes testability and modularity.      |

---

### 3.4. GoF Patterns

| Pattern              | Usage                                                            | Explanation                                     |
|----------------------|------------------------------------------------------------------|-------------------------------------------------|
| **Controller**       | `ManageVesselVisitController` coordinates UI and services.       | Mediates between presentation and domain.       |
| **Repository**       | `IVesselVisitRepository` handles persistence operations.         | Abstracts database access.                      |
| **State**            | The notification status transitions follow a finite state model. | Controls allowed operations based on state.     |
| **Observer / Event** | `AuditService` reacts to modifications.                          | Ensures change tracking without tight coupling. |

---
