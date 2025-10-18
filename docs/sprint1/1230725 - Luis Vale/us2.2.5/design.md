## 3. Design

### 3.1. System Sequence Diagram (SSD)

![ssdUS2.2.5.svg](CreateObject.svg)

---

### 3.2. Interaction Responsibility Assignment

| Interaction Step                                        | Question: Which class is responsible for…      | Class                     | Justification (with patterns)                                              |
|---------------------------------------------------------|------------------------------------------------|---------------------------|----------------------------------------------------------------------------|
| Step 1: Officer requests to register a new organization | Handling user input and triggering the process | `Swagger`                 | **Pure Fabrication:** Handles interaction and delegates to controller.     |
| Step 2: UI forwards the registration request            | Coordinating business logic                    | `OrganizationController`  | **Controller:** Coordinates actions and delegates to application services. |
| Step 3: Data validation and persistence                 | Validating input and executing business rules  | `OrganizationService`     | **Information Expert:** Centralizes the business logic.                    |
| Step 4: Persisting data                                 | Saving organization and representatives        | `IOrganizationRepository` | **Repository Pattern:** Handles data persistence logic.                    |
| Step 5: Feedback to user                                | Showing success message                        | `Swagger`                 | **Pure Fabrication:** Manages user interaction and result display.         |

---

### 3.3. Sequence Diagram

![sdUS2.2.5.svg](SD_US2.2.5_REGISTER_ORGANIZATION.svg)

### 3.4. SOLID Principles

| Principle                                     | Application in US_2.2.5                                                                             | Explanation                                         |
|-----------------------------------------------|-----------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `OrganizationController` handles only coordination of business logic.                               | Keeps UI, logic, and persistence separated.         |
|                                               | `OrganizationService` validates and processes business rules.                                       | Focused solely on registration logic.               |
|                                               | `Organization` and `Representative` encapsulate domain logic.                                       | Isolate business entities from infrastructure.      |
| **O – Open/Closed Principle (OCP)**           | Services and domain classes can be extended with new validations without changing existing logic.   | Allows extensibility with minimal modification.     |
| **L – Liskov Substitution Principle (LSP)**   | `IOrganizationRepository` can be replaced by any implementation (e.g., SQL, in-memory).             | Promotes interchangeable components.                |
| **I – Interface Segregation Principle (ISP)** | Interfaces like `IOrganizationRepository` are focused on essential operations (`save`, `findById`). | Prevents unnecessary dependencies.                  |
| **D – Dependency Inversion Principle (DIP)**  | The controller depends on abstractions (`IOrganizationRepository`).                                 | Promotes decoupling and enables testing with mocks. |

---

### 3.5. GoF Patterns

| Pattern                          | Usage in US_2.2.5                                                                  | Explanation                                       |
|----------------------------------|------------------------------------------------------------------------------------|---------------------------------------------------|
| **Controller**                   | `OrganizationController` coordinates the flow between UI, domain, and persistence. | Acts as mediator between layers.                  |
| **Repository**                   | `IOrganizationRepository` encapsulates persistence operations.                     | Abstracts database access.                        |
| **Information Expert**           | `Organization` and `Representative` contain their own validation logic.            | Domain entities hold relevant knowledge.          |
| **Pure Fabrication**             | Utilization of `Dtos` exist for technical separation of concerns.                  | SImproves maintainability and testability.        |
| **Low Coupling / High Cohesion** | Clear separation between layers and well-defined responsibilities.                 | Reduces interdependencies and increases cohesion. |

---