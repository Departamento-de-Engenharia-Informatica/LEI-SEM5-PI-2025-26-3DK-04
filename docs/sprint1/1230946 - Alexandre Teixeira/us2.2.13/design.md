## 3. Design

### 3.1. System Sequence Diagram (SSD)



---

### 3.2. Class Diagram 



---

### 3.3. Interaction Responsibility Assignment

| Interaction Step                                                  | Question: Which class is responsible for…      | Class                                    | Justification (with patterns)                                              |
|-------------------------------------------------------------------|------------------------------------------------|------------------------------------------|----------------------------------------------------------------------------|
| Step 1: Operator requests to create/update qualification         | Handling user input and triggering the process | `ManageQualificationUI`                  | **Pure Fabrication:** Handles interaction and delegates to controller.     |
| Step 2: UI forwards the management request                        | Coordinating business logic                    | `QualificationController`                | **Controller:** Coordinates actions and delegates to application services. |
| Step 3: Validate qualification data and business rules            | Validating input and executing business rules  | `QualificationService`                   | **Information Expert:** Centralizes the business logic.                    |
| Step 4: Check for unique qualification code                       | Verifying uniqueness of qualification code     | `IQualificationRepository`               | **Repository Pattern:** Handles data queries.                              |
| Step 5: Create/update qualification entity                        | Managing qualification data (code and name)    | `Qualification`                          | **Information Expert:** Domain entity holds relevant knowledge.            |
| Step 6: Persist qualification data                                | Saving qualification (create/update)           | `IQualificationRepository`               | **Repository Pattern:** Handles data persistence logic.                    |
| Step 7: Search/filter qualifications                              | Querying qualifications by code or name        | `IQualificationRepository`               | **Repository Pattern:** Provides search and filter operations.             |
| Step 8: Feedback to operator                                      | Showing success/error message                  | `ManageQualificationUI`                  | **Pure Fabrication:** Manages user interaction and result display.         |

---

### 3.4. Sequence Diagram



### 3.5. SOLID Principles

| Principle                                     | Application in US_2.2.13                                                                                         | Explanation                                         |
|-----------------------------------------------|------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `QualificationController` handles only coordination of business logic.                                           | Keeps UI, logic, and persistence separated.         |
|                                               | `QualificationService` validates and processes business rules for qualification management operations.           | Focused solely on qualification management logic.   |
|                                               | `Qualification` encapsulates domain logic for qualification code and descriptive name.                           | Isolate business entities from infrastructure.      |
| **O – Open/Closed Principle (OCP)**           | Services and domain classes can be extended with new qualification types without changing existing logic.        | Allows extensibility with minimal modification.     |
| **L – Liskov Substitution Principle (LSP)**   | `IQualificationRepository` can be replaced by any implementation (e.g., SQL, in-memory, NoSQL).                  | Promotes interchangeable components.                |
| **I – Interface Segregation Principle (ISP)** | Interfaces like `IQualificationRepository` are focused on essential operations (`save`, `findByCode`, `search`). | Prevents unnecessary dependencies.                  |
| **D – Dependency Inversion Principle (DIP)**  | The controller depends on abstractions (`IQualificationRepository`, `QualificationService`).                     | Promotes decoupling and enables testing with mocks. |

---

### 3.6. GoF Patterns

| Pattern                          | Usage in US_2.2.13                                                                                               | Explanation                                       |
|----------------------------------|------------------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| **Controller**                   | `QualificationController` coordinates the flow between UI, domain, and persistence.                              | Acts as mediator between layers.                  |
| **Repository**                   | `IQualificationRepository` encapsulates persistence operations for qualifications.                               | Abstracts database access.                        |
| **Information Expert**           | `Qualification` contains its own validation logic for code uniqueness and descriptive name.                      | Domain entities hold relevant knowledge.          |
| **Pure Fabrication**             | `ManageQualificationUI` and `QualificationService` exist for technical separation of concerns.                   | Improves maintainability and testability.         |
| **Strategy Pattern** (optional)  | Different search/filter strategies can be applied for querying qualifications by code or name.                   | Enables flexible search criteria.                 |
| **Low Coupling / High Cohesion** | Clear separation between layers and well-defined responsibilities.                                               | Reduces interdependencies and increases cohesion. |

---