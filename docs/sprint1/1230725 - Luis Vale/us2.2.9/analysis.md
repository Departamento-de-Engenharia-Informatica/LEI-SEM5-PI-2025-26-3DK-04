## 2. Analysis

### 2.1. User Story Overview

**ID:** US_2.2.9  
**Title:** Change or Complete a Vessel Visit Notification

**Description:**  
As a Shipping Agent Representative, I want to modify or complete a Vessel Visit Notification while it is still in
progress, allowing correction of errors or withdrawal before submission.

---

### Stakeholders

- **Primary Actor:** Shipping Agent Representative
- **Other Stakeholders:** Port Authority Officers, Vessel Management System

---

### 2.2. Business Rules

1. **Status Control:**
- Notifications in “in progress” status can be edited.
- Notifications marked as “submitted” become read-only.
2. **Data Integrity:**
- All changes must be logged with timestamp and user ID.
3. **Withdrawal Rule:**
- Representatives can withdraw notifications prior to approval.
4. **Validation:**
- Submission triggers validation of all required fields.

---
