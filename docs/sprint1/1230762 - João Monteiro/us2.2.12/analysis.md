## 2. Analysis

### 2.1. User Story Overview

**ID:** US_2.2.12  
**Title:** Register Physical Resources

**Description:**  
As a Logistics Operator, I want to register and manage physical resources so that they can be accurately considered during planning and scheduling operations.  
Resources include cranes, trucks, and other equipment, each with specific capacity, setup time, and qualification requirements.

---

### Stakeholders

- **Primary Actor:** Logistics Operator
- **Other Stakeholders:** Port Authority Officer, Operating Staff

---

### 2.2. Business Rules

1. **User Authentication:**
    - Only authenticated users with the role *Logistics Operator* can manage physical resources.
2. **Data Validation:**
    - Resource codes must be unique.
    - Qualification requirements must reference valid qualifications.
    - Setup time must be numeric and non-negative.
3. **Status Management:**
    - Resources can be marked as active, inactive, or under maintenance.
    - Deactivation must retain historical data for audit purposes.

---