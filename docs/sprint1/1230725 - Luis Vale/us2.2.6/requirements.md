# US_2.2.6 – Register and Manage Representatives of a Shipping Agent Organization

## 1. Requirements

### 1.1. User Story

**As a Port Authority Officer**,  
I want to register and manage representatives of a shipping agent organization (create, update, deactivate),  
so that the right individuals are authorized to interact with the system on behalf of their organization.

---

### 1.2. Customer Specifications and Clarifications

**From the specification document:**

- “As a Port Authority Officer, I want to register and manage representatives of a shipping agent organization (create,
  update, deactivate), so that the right individuals are authorized to interact with the system on behalf of their
  organization.”
- Each representative must be associated with exactly one shipping agent organization.
- Required representative details include: name, citizen ID, nationality, email, and phone number.

**Clarifications:**


---

### 1.3. Acceptance Criteria

- Each representative must belong to one and only one organization.
- The system must allow the creation, update, and deactivation of representatives.
- Duplicate emails or citizen IDs must not be allowed.
- Deactivation must retain the representative’s history and audit data.
- The system must send notifications upon creation or deactivation.

---

### 1.4. Found Dependencies

- US_2.2.5 – Register Shipping Agent Organization
- Authentication / Authorization module

---

### 1.5. Other Relevant Remarks

- The management of representatives follows the same dependency flow (UI → Application → Domain → Infrastructure).
- The functionality must comply with the layered architecture principles defined in *Something About Layered
  Architecture Patterns v8*.

---
