
## 3. Design

### 3.1. System Sequence Diagram (SSD)



---

### 3.2. Class Diagram (Conceptual)



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