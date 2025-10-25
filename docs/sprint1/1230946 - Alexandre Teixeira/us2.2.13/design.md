## 3. Design

### 3.1. System Sequence Diagram (SSD)
![alt text](<../../level1/process_view/1230946 - Alexandre Texeira/US2.2.13/SD_US2.2.13_CREATE.svg>)
![alt text](<../../level1/process_view/1230946 - Alexandre Texeira/US2.2.13/SD_US2.2.13_UPDATE.svg>)


---

### 3.2. Interaction Responsibility Assignment

| Interaction Step                                                  | Question: Which class is responsible for…      | Class / Method                            | Justification (with patterns)                                              |
|-------------------------------------------------------------------|------------------------------------------------|------------------------------------------|----------------------------------------------------------------------------|
| Step 1: Operator requests create/update qualification             | Which class handles the HTTP request?          | `QualificationsController`               | **Controller:** HTTP entry point; validates request and delegates.        |
| Step 2: Controller forwards the request                           | Which class coordinates business rules?        | `QualificationService`                   | **Service / Facade:** Orchestrates repositories and domain operations.     |
| Step 3: Validate uniqueness (qualification code)                  | Which class verifies constraints against storage? | `IQualificationRepository` / `QualificationRepository` | **Repository:** Performs queries and uniqueness checks.                    |
| Step 4: Create/update qualification entity                        | Which class owns qualification data & rules?   | `Qualification` (aggregate)              | **Information Expert / Aggregate Root:** Enforces invariants and behavior.|
| Step 5: Persist changes                                            | Which classes commit data to the DB?           | `IQualificationRepository` + `UnitOfWork.CommitAsync()` | **Repository + Unit of Work:** Encapsulate persistence and transactions.   |
| Step 6: Search / filter qualifications                             | Which class exposes search/filter operations?  | `QualificationRepository`                 | **Repository:** Provides query methods (search by code/name).             |
| Step 7: Feedback to operator                                       | Which class returns HTTP responses?            | `QualificationsController`               | **Controller:** Maps service results to HTTP responses.                    |

---

### 3.3. Sequence Diagram
![alt text](../../level3/process_view/createObject/CreateObject.svg)
![alt text](../../level3/process_view/updateObject/UpdateObject.svg)


### 3.4. SOLID Principles

| Principle                                     | Application in US_2.2.13                                                                                         | Explanation                                         |
|-----------------------------------------------|------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `QualificationsController` — HTTP layer; `QualificationService` — application orchestration; `Qualification` — domain rules; `IQualificationRepository` — persistence | Each type has one clear responsibility and is easier to test. |
| **O – Open/Closed Principle (OCP)**           | `QualificationService` and `Qualification` are open for extension (new validation rules or attributes) without modifying callers. | New qualification types or rules can be added with minimal changes. |
| **L – Liskov Substitution Principle (LSP)**   | `IQualificationRepository` implementations (EF Core, in-memory) can be swapped in tests and runtime.            | Repositories can be replaced without breaking clients. |
| **I – Interface Segregation Principle (ISP)** | `IQualificationRepository` provides focused methods (save, findByCode, search) while DTOs keep contracts small.   | Consumers depend only on required operations.       |
| **D – Dependency Inversion Principle (DIP)**  | High-level modules depend on abstractions (`IQualificationRepository`, service interfaces) injected into the controller/service. | Promotes decoupling and enables mocking for tests.  |

---

### 3.5. GoF Patterns

| Pattern                          | Usage in US_2.2.13                                                                                               | Explanation                                       |
|----------------------------------|------------------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| **Controller**                   | `QualificationsController` — exposes REST endpoints and delegates to the service.                                 | Separates transport concerns from business logic. |
| **Repository**                   | `IQualificationRepository` / `QualificationRepository` — encapsulate data access and search operations.           | Hides persistence details and provides collection-like API. |
| **Information Expert**           | `Qualification` aggregate — implements validation (code uniqueness, name rules) and behavior for the entity.     | Places behavior next to the data that owns it.    |
| **Pure Fabrication**             | DTOs (`CreateQualificationDto`, `QualificationDto`) and the service layer (`QualificationService`) separate technical concerns (mapping, orchestration) from the domain. | Improves testability and keeps domain focused.    |
| **Strategy (optional)**          | Search/filter strategies (by code, name or pagination) can be implemented in repository helpers or query objects. | Enables flexible, testable query implementations. |
| **Low Coupling / High Cohesion** | Clear Controller → Service → Repository separation with DI and focused interfaces.                               | Keeps modules focused and easy to test/change.    |

---