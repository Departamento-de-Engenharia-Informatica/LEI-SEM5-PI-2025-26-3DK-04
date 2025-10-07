
## 2. Analysis

### 2.1. User Story Overview

**ID:** US_2.2.5  
**Title:** Register Shipping Agent Organizations

**Description:**  
As a Port Authority Officer, I want to register new shipping agent organizations so that they can operate within the
port’s digital system.  
Each organization includes identifiers, names, address, tax number, and representatives responsible for communication
and authentication.

---

### Stakeholders

- **Primary Actor:** Port Authority Officer
- **Other Stakeholders:** Port Authority Administration, Shipping Agents

---

### 2.2. Business Rules

1. **User Authentication:**
    - Only authenticated users with the role *Port Authority Officer* can register shipping agents.
2. **Data Consistency:**
    - Each organization must include at least one representative.
    - Each representative must have a unique email and citizen ID.
    - Organization tax numbers must be unique in the system.
3. **Notification:**
    - Upon successful registration, an automatic email must be sent to all representatives.

---
