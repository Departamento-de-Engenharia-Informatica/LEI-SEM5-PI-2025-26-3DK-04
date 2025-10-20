# US_2.2.4 – Register/Update Storage Areas

## 1. Requirements

### 1.1. User Story

**As a Port Authority Officer**,  
I want to register and update storage areas  
so that (un)loading and storage operations can be assigned to the correct locations.

---

### 1.2. Customer Specifications and Clarifications

**From the specification document:**

- “As a Port Authority Officer, I want to register and update storage areas so that (un)loading and storage operations can be assigned to the correct locations.”
- Each storage area must have a unique identifier, type (e.g., yard, warehouse), and location within the port.
- Storage areas must specify maximum capacity (in TEUs) and current occupancy.
- By default, a storage area serves the entire port. However, some yards may be constrained to serve only specific docks.
- Complementary information, such as the distance between docks and storage areas, must be manually recorded to support future logistics planning and optimization.
- Updates must not allow current occupancy to exceed maximum capacity.

**Clarifications:**
>
> > **Question:**
> >1. When a Port Authority Officer is registering a storage area in the system he must manually insert information such distance between docks and storage areas. My question is if the distance that he must insert is to all the docks., it means, if the port has 5 docks he must insert 5 distances?
> >2. Other question is if it's necessary to keep the distance between storage areas.
>
> > **Answer:**
> >1. If the storage area serves all docks, you need to know those distances.
> >2. By the moment, that is not necessary.
---

### 1.3. Acceptance Criteria

- Storage areas must include all mandatory fields (ID, type, location, max capacity, current occupancy).
- The system must prevent updates that exceed maximum capacity.
- It must be possible to associate specific docks with storage areas.
- Distance metadata must be recordable and retrievable.
- The operation must respect the system’s layered architecture principles.

---

### 1.4. Found Dependencies

- US_2.2.3 – Register Docks
- US_2.2.12 – Register Physical Resources
- Authentication / Authorization module

---

### 1.5. Other Relevant Remarks

- The registration process follows the Clean Architecture dependency rule (UI → Application → Domain).
- Data persistence and distance metadata are handled by infrastructure components abstracted through repository and service interfaces.

---
