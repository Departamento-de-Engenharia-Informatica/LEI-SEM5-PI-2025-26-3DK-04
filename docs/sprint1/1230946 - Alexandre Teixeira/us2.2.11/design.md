## 3. Design

### 3.1. System Sequence Diagram (SSD)



---

### 3.2. Class Diagram 



---

### 3.3. Interaction Responsibility Assignment

| Interaction Step                                                  | Question: Which class is responsible for…      | Class                                    | Justification (with patterns)                                              |
|-------------------------------------------------------------------|------------------------------------------------|------------------------------------------|----------------------------------------------------------------------------|
| Step 1: Operator requests to create/update/deactivate staff      | Handling user input and triggering the process | `ManageStaffMemberUI`                    | **Pure Fabrication:** Handles interaction and delegates to controller.     |
| Step 2: UI forwards the management request                        | Coordinating business logic                    | `StaffMemberController`                  | **Controller:** Coordinates actions and delegates to application services. |
| Step 3: Validate staff data and business rules                    | Validating input and executing business rules  | `StaffMemberService`                     | **Information Expert:** Centralizes the business logic.                    |
| Step 4: Check for unique mecanographic number                     | Verifying uniqueness of staff ID               | `StaffMemberRepository`                 | **Repository Pattern:** Handles data queries.                              |
| Step 5: Create/update staff member entity                         | Managing staff member data and status          | `OperatingStaffMember`                   | **Information Expert:** Domain entity holds relevant knowledge.            |
| Step 6: Persist staff member data                                 | Saving staff member (create/update/deactivate) | `StaffMemberRepository`                 | **Repository Pattern:** Handles data persistence logic.                    |
| Step 7: Search/filter staff members                               | Querying staff by criteria                     | `StaffMemberRepository`                 | **Repository Pattern:** Provides search and filter operations.             |
| Step 8: Feedback to operator                                      | Showing success/error message                  | `ManageStaffMemberUI`                    | **Pure Fabrication:** Manages user interaction and result display.         |

---

### 3.4. Sequence Diagram



### 3.5. SOLID Principles

| Principle                                     | Application in US_2.2.11                                                                                         | Explanation                                         |
|-----------------------------------------------|------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `StaffMemberController` handles only coordination of business logic.                                             | Keeps UI, logic, and persistence separated.         |
|                                               | `StaffMemberService` validates and processes business rules for staff management operations.                     | Focused solely on staff management logic.           |
|                                               | `OperatingStaffMember` encapsulates domain logic for staff data, qualifications, and status.                     | Isolate business entities from infrastructure.      |
| **O – Open/Closed Principle (OCP)**           | Services and domain classes can be extended with new qualifications or status types without changing logic.      | Allows extensibility with minimal modification.     |
| **L – Liskov Substitution Principle (LSP)**   | `StaffMemberRepository` can be replaced by any implementation (e.g., SQL, in-memory, NoSQL).                    | Promotes interchangeable components.                |
| **I – Interface Segregation Principle (ISP)** | Interfaces like `StaffMemberRepository` are focused on essential operations (`save`, `findById`, `search`).     | Prevents unnecessary dependencies.                  |
| **D – Dependency Inversion Principle (DIP)**  | The controller depends on abstractions (`StaffMemberRepository`, `StaffMemberService`).                         | Promotes decoupling and enables testing with mocks. |

---

### 3.6. GoF Patterns

| Pattern                          | Usage in US_2.2.11                                                                                               | Explanation                                       |
|----------------------------------|------------------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| **Controller**                   | `StaffMemberController` coordinates the flow between UI, domain, and persistence.                                | Acts as mediator between layers.                  |
| **Repository**                   | `StaffMemberRepository` encapsulates persistence operations for staff members.                                  | Abstracts database access.                        |
| **Information Expert**           | `OperatingStaffMember` contains its own validation logic for staff data, qualifications, and status.             | Domain entities hold relevant knowledge.          |
| **Pure Fabrication**             | `ManageStaffMemberUI` and `StaffMemberService` exist for technical separation of concerns.                       | Improves maintainability and testability.         |
| **State Pattern** (optional)     | `OperatingStaffMember` status transitions (available ↔ unavailable) can be modeled using state pattern.          | Manages different staff status states cleanly.    |
| **Strategy Pattern** (optional)  | Different search/filter strategies can be applied for querying staff members.                                    | Enables flexible search criteria.                 |
| **Low Coupling / High Cohesion** | Clear separation between layers and well-defined responsibilities.                                               | Reduces interdependencies and increases cohesion. |

---