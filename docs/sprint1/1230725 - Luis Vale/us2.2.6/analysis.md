
## 2. Analysis

### 2.1. User Story Overview

**ID:** US_2.2.6  
**Title:** Register and Manage Representatives of a Shipping Agent Organization

**Description:**  
As a Port Authority Officer, I want to create, update, and deactivate representatives of shipping agent organizations,
ensuring that only authorized individuals can act on behalf of their company within the system.

---

### Stakeholders

- **Primary Actor:** Port Authority Officer
- **Other Stakeholders:** Shipping Agents, Port Authority Administration

---

### 2.2. Business Rules

1. **Association Rule:**
   - A representative must be linked to exactly one organization.
2. **Data Validation:**
   - Required fields: name, citizen ID, nationality, email, phone.
   - Citizen ID and email must be unique.
3. **Deactivation Rule:**
   - Deactivated representatives remain in the system but cannot authenticate.
4. **Notification Rule:**
   - Representatives and their organizations are notified upon creation, update, or deactivation.

---
