## 3. Design

### 3.1. System Sequence Diagram (SSD)
![alt text](<../../level1/process_view/1230762%20-%20João%20Monteiro/US%202.2.4/SD_US2_2_4_CREATE.svg>)

![alt text](<../../level1/process_view/1230762%20-%20João%20Monteiro/US%202.2.4/SD_US2_2_4_UPDATE.svg>)


---

### 3.2. Class Diagram (Conceptual)

**StorageArea**

- id, type, location, maxCapacity, currentOccupancy

**Dock**

- id, name, location

**DistanceMetadata**

- dockId, storageAreaId, distanceInMeters

**Relationship:**  
`StorageArea 1..* — 0..* Dock`  
`DistanceMetadata 1 — 1 StorageArea-Dock pair`

---

### 3.3. Interaction Responsibility Assignment

| Interaction Step                                     | Question: Which class is responsible for…      | Class                          | Justification (with patterns)                                               |
|------------------------------------------------------|------------------------------------------------|--------------------------------|----------------------------------------------------------------------------|
| Step 1: Officer requests to register a storage area  | Handling user input and triggering the process | `RegisterStorageAreaUI`        | **Pure Fabrication:** Manages interaction and delegates to controller.     |
| Step 2: UI forwards the registration request         | Coordinating business logic                    | `RegisterStorageAreaController`| **Controller:** Coordinates actions and delegates to application services. |
| Step 3: Data validation and persistence              | Validating input and executing business rules  | `RegisterStorageAreaService`   | **Information Expert:** Centralizes the business logic.                    |
| Step 4: Persisting data                              | Saving storage area and dock associations      | `IStorageAreaRepository`       | **Repository Pattern:** Handles data persistence logic.                    |
| Step 5: Feedback to user                             | Showing success message                        | `RegisterStorageAreaUI`        | **Pure Fabrication:** Manages user interaction and result display.         |

---

### 3.4. Sequence Diagram

![alt text](../../level3/process_view/createObject/CreateObject.svg)
![alt text](../../level3/process_view/updateObject/UpdateObject.svg)

---

### 3.5. SOLID Principles

| Principle                                     | Application in US_2.2.4                                                                              | Explanation                                         |
|----------------------------------------------|------------------------------------------------------------------------------------------------------|-----------------------------------------------------|
| **S – Single Responsibility Principle (SRP)** | `RegisterStorageAreaController` handles only coordination of business logic.                         | Keeps UI, logic, and persistence separated.         |
|                                              | `RegisterStorageAreaService` validates and processes business rules.                                 | Focused solely on storage area logic.               |
|                                              | `StorageArea` encapsulates domain logic.                                                             | Isolates business entities from infrastructure.     |
| **O – Open/Closed Principle (OCP)**           | Services and domain classes can be extended with new validations without changing existing logic.    | Allows extensibility with minimal modification.     |
| **L – Liskov Substitution Principle (LSP)**   | `IStorageAreaRepository` can be replaced by any implementation (e.g., SQL, in-memory).               | Promotes interchangeable components.                |
| **I – Interface Segregation Principle (ISP)** | Interfaces like `IStorageAreaRepository` are focused on essential operations (`save`, `findById`).   | Prevents unnecessary dependencies.                  |
| **D – Dependency Inversion Principle (DIP)**  | The controller depends on abstractions (`IStorageAreaRepository`).                                   | Promotes decoupling and enables testing with mocks. |

---

### 3.6. GoF Patterns

| Pattern                          | Usage in US_2.2.4                                                                                     | Explanation                                       |
|----------------------------------|--------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| **Controller**                   | `RegisterStorageAreaController` coordinates the flow between UI, domain, and persistence.             | Acts as mediator between layers.                  |
| **Repository**                   | `IStorageAreaRepository` encapsulates persistence operations.                                          | Abstracts database access.                        |
| **Information Expert**           | `StorageArea` contains its own validation logic.                                                       | Domain entity holds relevant knowledge.           |
| **Pure Fabrication**             | `RegisterStorageAreaUI` exists for technical separation of concerns.                                   | Improves maintainability and testability.         |
| **Low Coupling / High Cohesion**| Clear separation between layers and well-defined responsibilities.                                     | Reduces interdependencies and increases cohesion. |

---