## 2. Analysis

### 2.1. User Story Overview

**ID:** US_2.2.4  
**Title:** Register Storage Areas

**Description:**  
As a Port Authority Officer, I want to register and update storage areas so that (un)loading and storage operations can be assigned to the correct locations.  
Each storage area includes identifiers, type, location, capacity, occupancy, and optional dock associations.

---

### Stakeholders

- **Primary Actor:** Port Authority Officer
- **Other Stakeholders:** Logistics Operators, Port Administration

---

### 2.2. Business Rules

1. **User Authentication:**
    - Only authenticated users with the role *Port Authority Officer* can register storage areas.
2. **Data Consistency:**
    - Each storage area must have a unique identifier.
    - Maximum capacity must be greater than or equal to current occupancy.
    - Dock associations must reference valid dock records.
3. **Distance Metadata:**
    - Manual input of distance between docks and storage areas must be supported.

---