## 3. Design

### 3.1. System Sequence Diagram (SSD)
![alt text](<../../level1/process_view/1230762%20-%20João%20Monteiro/US%202.2.3/SD_US2_2_3_CREATE.svg>)

![alt text](<../../level1/process_view/1230762%20-%20João%20Monteiro/US%202.2.3/SD_US2_2_3_UPDATE.svg>)


---

### 3.2. Interaction Responsibility Assignment

| Interaction Step                                 | Question: Which class is responsible for…      | Class                         | Justification (with patterns)                                               |
|--------------------------------------------------|------------------------------------------------|-------------------------------|----------------------------------------------------------------------------|
| Step 1: Officer requests to register a dock      | Handling user input and triggering the process | `RegisterDockUI`              | **Pure Fabrication:** Manages interaction and delegates to controller.     |
| Step 2: UI forwards the registration request     | Coordinating business logic                    | `RegisterDockController`      | **Controller:** Coordinates actions and delegates to application services. |
| Step 3: Data validation and persistence          | Validating input and executing business rules  | `RegisterDockService`         | **Information Expert:** Centralizes the business logic.                    |
| Step 4: Persisting data                          | Saving dock and vessel type associations       | `IDockRepository`             | **Repository Pattern:** Handles data persistence logic.                    |
| Step 5: Feedback to user                         | Showing success message                        | `RegisterDockUI`              | **Pure Fabrication:** Manages user interaction and result display.         |

---

### 3.3. Sequence Diagram
![alt text](../../level3/process_view/createObject/CreateObject.svg)


### 3.4. SOLID Principles

| Principle                                     | Application in US_2.2.3                                                                              | Explanation                                         |
|----------------------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `RegisterDockController` handles only coordination of business logic.                               | Keeps UI, logic, and persistence separated.         |
|                                              | `RegisterDockService` validates and processes business rules.                                       | Focused solely on dock registration logic.          |
|                                              | `Dock` encapsulates domain logic.                                                                   | Isolates business entities from infrastructure.     |
| **O – Open/Closed Principle (OCP)**           | Services and domain classes can be extended with new validations without changing existing logic.    | Allows extensibility with minimal modification.     |
| **L – Liskov Substitution Principle (LSP)**   | `IDockRepository` can be replaced by any implementation (e.g., SQL, in-memory).                      | Promotes interchangeable components.                |
| **I – Interface Segregation Principle (ISP)** | Interfaces like `IDockRepository` are focused on essential operations (`save`, `findById`).          | Prevents unnecessary dependencies.                  |
| **D – Dependency Inversion Principle (DIP)**  | The controller depends on abstractions (`IDockRepository`).                                          | Promotes decoupling and enables testing with mocks. |

---

### 3.5. GoF Patterns

| Pattern                          | Usage in US_2.2.3                                                                                    | Explanation                                       |
|----------------------------------|------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| **Controller**                   | `RegisterDockController` coordinates the flow between UI, domain, and persistence.                  | Acts as mediator between layers.                  |
| **Repository**                   | `IDockRepository` encapsulates persistence operations.                                               | Abstracts database access.                        |
| **Information Expert**           | `Dock` contains its own validation logic.                                                            | Domain entity holds relevant knowledge.           |
| **Pure Fabrication**             | `RegisterDockUI` exists for technical separation of concerns.                                        | Improves maintainability and testability.         |
| **Low Coupling / High Cohesion**| Clear separation between layers and well-defined responsibilities.                                   | Reduces interdependencies and increases cohesion. |

---