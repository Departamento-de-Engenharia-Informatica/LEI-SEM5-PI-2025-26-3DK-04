## 2. Analysis

### 2.1. User Story Overview

**ID:** US_2.2.8  
**Title:** Submit Vessel Visit Notification

**Description:**  
As a Shipping Agent Representative, I want to create and submit a Vessel Visit Notification so that the vessel berthing and subsequent (un)loading operations at the port are scheduled and planned.  
The notification includes vessel details, cargo manifests, and crew information when applicable.

---

### Stakeholders

- **Primary Actor:** Shipping Agent Representative
- **Other Stakeholders:** Port Authority Officer, Logistics Operator

---

### 2.2. Business Rules

1. **User Authentication:**
    - Only authenticated users with the role *Shipping Agent Representative* can submit notifications.
2. **Data Validation:**
    - Container IDs must comply with ISO 6346:2022.
    - Crew safety officers must be identified for hazardous cargo.
3. **Status Management:**
    - Notification starts as "in progress" and must be changed to "submitted" to trigger approval workflow.

---
