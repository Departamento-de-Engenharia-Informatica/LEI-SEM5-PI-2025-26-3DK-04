# US_2.2.5 – Register Shipping Agent Organizations

## 1. Requirements

### 1.1. User Story

**As a Port Authority Officer**,  
I want to register new shipping agent organizations  
so that they can operate within the port’s digital system.

---

### 1.2. Customer Specifications and Clarifications

**From the specification document:**

- “As a Port Authority Officer, I want to register new shipping agent organizations so that they can operate within the
  port’s digital system.”
- Each organization must include at least an identifier, legal and alternative names, an address, and a tax number.
- Each organization must include at least one representative at the time of registration.
- Representatives must be registered with name, citizen ID, nationality, email, and phone number.
- Email and phone number are used for system notifications, including approval decisions and authentication.

**Clarifications:**



---

### 1.3. Acceptance Criteria

- Each organization must contain all mandatory fields (ID, name, address, tax number).
- At least one representative must be added at the time of registration.
- Representatives must include valid contact information (email, phone).
- A confirmation email must be sent to the registered representative.
- The operation must respect the system’s layered architecture principles.

---

### 1.4. Found Dependencies

- US_2.2.6 – Edit Shipping Agent
- US_2.2.7 – Approve Shipping Agent
- Authentication / Authorization module

---

### 1.5. Other Relevant Remarks

- The registration process follows the dependency rule defined in the Clean Architecture approach (UI → Application →
  Domain).
- Data persistence and notifications are handled by infrastructure components, abstracted through repository and service
  interfaces.

---