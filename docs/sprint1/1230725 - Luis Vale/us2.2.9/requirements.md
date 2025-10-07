# US_2.2.9 – Change or Complete a Vessel Visit Notification

## 1. Requirements

### 1.1. User Story

**As a Shipping Agent Representative**,  
I want to change or complete a Vessel Visit Notification while it is still in progress,  
so that I can correct errors or withdraw requests if necessary.

---

### 1.2. Customer Specifications and Clarifications

**From the specification document:**

- “As a Shipping Agent Representative, I want to change / complete a Vessel Visit Notification while it is still in
  progress, so that I can correct errors or withdraw requests if necessary.”
- Status can be maintained as “in progress” or changed to “submitted / approval pending” by the representative.

**Clarifications:**


---

### 1.3. Acceptance Criteria

- Representatives can modify details while the status is “in progress.”
- When the representative submits the notification, the status changes to “submitted / approval pending.”
- Once submitted, the notification cannot be edited but can be withdrawn.
- All modifications must be recorded with timestamps and user identification.

---

### 1.4. Found Dependencies

- US_2.2.8 – Create Vessel Visit Notification
- Authentication / Authorization module


---

### 1.5. Other Relevant Remarks

- The notification’s lifecycle is managed by status transitions: **In Progress → Submitted / Approval Pending →
  Approved / Rejected**.
- Must comply with the dependency inversion and separation of concerns principles described in *Layered Architecture
  Patterns v8*.

---

