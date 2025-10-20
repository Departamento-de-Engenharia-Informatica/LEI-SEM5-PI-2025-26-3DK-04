## 2. Analysis

### 2.1. User Story Overview

**ID:** US_2.2.3  
**Title:** Register Docks

**Description:**  
As a Port Authority Officer, I want to register and update docks so that the system accurately reflects the docking capacity of the port.  
Each dock includes identifiers, location, physical characteristics, and allowed vessel types.

---

### Stakeholders

- **Primary Actor:** Port Authority Officer
- **Other Stakeholders:** Port Authority Administration, Shipping Agents

---

### 2.2. Business Rules

1. **User Authentication:**
    - Only authenticated users with the role *Port Authority Officer* can register docks.
2. **Data Consistency:**
    - Each dock must have a unique identifier.
    - Vessel types assigned to a dock must exist in the system.
3. **Searchability:**
    - Docks must be searchable by name, vessel type, and location.

---