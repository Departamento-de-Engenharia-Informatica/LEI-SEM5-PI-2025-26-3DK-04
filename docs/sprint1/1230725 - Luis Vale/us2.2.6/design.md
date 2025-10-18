## 3. Design

### 3.1. System Sequence Diagram (SSD)

#### Create/register representative

![sdUS2.2.6.svg](CreateObject.svg)

#### Activate representative

![sdUS2.2.6.svg](ActivateObject.svg)

#### Deactivate representative

![sdUS2.2.6.svg](DeactivateObject.svg)

#### Update representative

![sdUS2.2.6.svg](UpdateObject.svg)
---

### 3.2. Interaction Responsibility Assignment — US 2.2.6 (Manage Shipping Agent Representatives)

| Interaction Step                                                  | Question: Which class is responsible for…                          | Class                       | Justification (with patterns)                                                                   |
|-------------------------------------------------------------------|--------------------------------------------------------------------|-----------------------------|-------------------------------------------------------------------------------------------------|
| Step 1: User requests to create or update a representative via UI | Handling user input and initiating the process                     | `Swagger`                   | **Pure Fabrication:** Handles user interaction and delegates API requests to the controller.    |
| Step 2: UI sends the HTTP request to backend                      | Coordinating incoming requests and forwarding to the service layer | `RepresentativesController` | **Controller:** Orchestrates actions and delegates business logic to the service layer.         |
| Step 3: Validate representative data and apply business rules     | Executing domain logic and validation                              | `RepresentativeService`     | **Information Expert:** Holds business rules such as unique email/CitizenID and activation.     |
| Step 4: Retrieve or persist representative data                   | Managing database access and persistence                           | `IRepresentativeRepository` | **Repository Pattern:** Encapsulates data access logic, providing abstraction from persistence. |
| Step 5: Commit changes to the database                            | Ensuring atomic data operations                                    | `UnitOfWork`                | **Unit of Work:** Guarantees transactional integrity during creation/update operations.         |
| Step 6: Return confirmation or error to the user                  | Displaying success or failure message                              | `Swagger`                   | **Pure Fabrication:** Displays operation result to the user.                                    |

### 3.3. Sequence Diagram (SD)

![sdUS2.2.6.svg](SD_US2.2.6_MANAGE_REPRESENTATIVES.svg)
---

### 3.4. SOLID Principles

| Principle                                     | Application                                                                                     | Explanation                                 |
|-----------------------------------------------|-------------------------------------------------------------------------------------------------|---------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `RepresentativeController` coordinates logic only.                                              | Separation of UI, service, and persistence. |
| **O – Open/Closed Principle (OCP)**           | Validation rules and representative operations can be extended without modifying existing code. | Extensible architecture.                    |
| **L – Liskov Substitution Principle (LSP)**   | Any implementation of `RepresentativeService` can be used.                                      | Supports polymorphism.                      |
| **I – Interface Segregation Principle (ISP)** | Service exposes only minimal methods (`create`, `update`, `deactivate`).                        | Prevents unnecessary dependencies.          |
| **D – Dependency Inversion Principle (DIP)**  | The controller depends on abstractions (`IRepresentativeRepository`)..                          | Enables loose coupling.                     |

---

### 3.5. GoF Patterns

| Pattern                          | Usage                                                              | Explanation                                       |
|----------------------------------|--------------------------------------------------------------------|---------------------------------------------------|
| **Controller**                   | `«RepresentativeController` orchestrates operations.               | Coordinates flow between layers.                  |
| **Repository**                   | `IRepresentativeRepository` abstracts data persistence.            | Manages access to the persistence layer.          |
| **Information Expert**           | `Representative` contains its validation and state logic.          | Encapsulates its own data and behavior.           |
| **Pure Fabrication**             | Utilization of `Dtos` exist for technical separation of concerns.  | SImproves maintainability and testability.        |
| **Low Coupling / High Cohesion** | Clear separation between layers and well-defined responsibilities. | Reduces interdependencies and increases cohesion. |

---