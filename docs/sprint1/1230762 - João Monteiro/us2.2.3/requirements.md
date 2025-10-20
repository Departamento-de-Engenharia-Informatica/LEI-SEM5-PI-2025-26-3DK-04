# US_2.2.3 – Register/Update Docks

## 1. Requirements

### 1.1. User Story

**As a Port Authority Officer**,  
I want to register and update docks  
so that the system accurately reflects the docking capacity of the port.

---

### 1.2. Customer Specifications and Clarifications

**From the specification document:**

- “As a Port Authority Officer, I want to register and update docks so that the system accurately reflects the docking capacity of the port.”
- A dock record must include a unique identifier, name/number, location within the port, and physical characteristics (e.g., length, depth, max draft).
- The officer must specify the vessel types allowed to berth there.
- Docks must be searchable and filterable by name, vessel type, and location.

**Clarifications:**
>
> > **Question:** Hello, I would like some clarification:
> >1. Which fields of a dock are allowed to be updated once it is registered?
> >2. Should the system maintain a log of dock updates, recording who made the changes and when?
>
> > **Answer:**
> >1. All excepting the id.
> >2. Yes, for compliance with the statement "All user interactions must be carefully logged, producing detailed records of every significant action
      performed in the system. These logs are not only essential for auditing and traceability but also serve
      as an important tool for diagnosing issues and analyzing user behavior." (cf. System Description document, section 3.3)
---

### 1.3. Acceptance Criteria

- Each dock must include all mandatory fields (ID, name, location, physical characteristics).
- Multiple vessel types must be assignable to a dock.
- Docks must be searchable and filterable by name, vessel type, and location.
- The operation must respect the system’s layered architecture principles.

---

### 1.4. Found Dependencies

- US_2.2.1 – Register Vessel Types
- Authentication / Authorization module

---

### 1.5. Other Relevant Remarks

- The registration process follows the Clean Architecture dependency rule (UI → Application → Domain).
- Data persistence is handled by infrastructure components abstracted through repository interfaces.

---