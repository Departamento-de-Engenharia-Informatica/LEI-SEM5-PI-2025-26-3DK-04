## 3. Design

### 3.1. System Sequence Diagram (SSD)



---

### 3.2. Class Diagram (Conceptual)

**PhysicalResource**

- code, description, type, capacity, status, setupTime, assignedArea

**Qualification**

- code, name

**Relationship:**  
`PhysicalResource 1 — 0..* Qualification`

---

### 3.3. Interaction Responsibility Assignment

| Interaction Step                                     | Question: Which class is responsible for…      | Class                             | Justification (with patterns)                                               |
|------------------------------------------------------|------------------------------------------------|-----------------------------------|----------------------------------------------------------------------------|
| Step 1: Operator initiates resource registration     | Handling user input and triggering the process | `RegisterResourceUI`              | **Pure Fabrication:** Manages interaction and delegates to controller.     |
| Step 2: UI forwards the registration request         | Coordinating business logic                    | `RegisterResourceController`      | **Controller:** Coordinates actions and delegates to application services. |
| Step 3: Data validation and persistence              | Validating input and executing business rules  | `RegisterResourceService`         | **Information Expert:** Centralizes the business logic.                    |
| Step 4: Persisting data                              | Saving resource and qualification links        | `IResourceRepository`             | **Repository Pattern:** Handles data persistence logic.                    |
| Step 5: Feedback to user                             | Showing success message                        | `RegisterResourceUI`              | **Pure Fabrication:** Manages user interaction and result display.         |

---

### 3.4. Sequence Diagram



---

### 3.5. SOLID Principles

| Principle                                     | Application in US_2.2.12                                                                             | Explanation                                         |
|----------------------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `RegisterResourceController` handles only coordination of business logic.                           | Keeps UI, logic, and persistence separated.         |
|                                              | `RegisterResourceService` validates and processes business rules.                                   | Focused solely on resource logic.                   |
|                                              | `PhysicalResource` encapsulates domain logic.                                                       | Isolates business entities from infrastructure.     |
| **O – Open/Closed Principle (OCP)**           | Services and domain classes can be extended with new validations without changing existing logic.    | Allows extensibility with minimal modification.     |
| **L – Liskov Substitution Principle (LSP)**   | `IResourceRepository` can be replaced by any implementation (e.g., SQL, in-memory).                  | Promotes interchangeable components.                |
| **I – Interface Segregation Principle (ISP)** | Interfaces like `IResourceRepository` are focused on essential operations (`save`, `findById`).      | Prevents unnecessary dependencies.                  |
| **D – Dependency Inversion Principle (DIP)**  | The controller depends on abstractions (`IResourceRepository`).                                      | Promotes decoupling and enables testing with mocks. |

---

### 3.6. GoF Patterns

| Pattern                          | Usage in US_2.2.12                                                                                    | Explanation                                       |
|----------------------------------|--------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| **Controller**                   | `RegisterResourceController` coordinates the flow between UI, domain, and persistence.                | Acts as mediator between layers.                  |
| **Repository**                   | `IResourceRepository` encapsulates persistence operations.                                             | Abstracts database access.                        |
| **Information Expert**           | `PhysicalResource` contains its own validation logic.                                                  | Domain entity holds relevant knowledge.           |
| **Pure Fabrication**             | `RegisterResourceUI` exists for technical separation of concerns.                                      | Improves maintainability and testability.         |
| **Low Coupling / High Cohesion**| Clear separation between layers and well-defined responsibilities.                                     | Reduces interdependencies and increases cohesion. |

---