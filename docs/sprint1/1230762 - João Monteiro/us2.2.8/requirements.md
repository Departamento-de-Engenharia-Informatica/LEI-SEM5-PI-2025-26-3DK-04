# US_2.2.8 – Submit Vessel Visit Notification

## 1. Requirements

### 1.1. User Story

**As a Shipping Agent Representative**,  
I want to create and submit a Vessel Visit Notification  
so that the vessel berthing and subsequent (un)loading operations at the port are scheduled and planned in space and timely manner.

---

### 1.2. Customer Specifications and Clarifications

**From the specification document:**

- “As a Shipping Agent Representative, I want to create/submit a Vessel Visit Notification so that the vessel berthing and subsequent (un)loading operations at the port are scheduled and planned in space and timely manner.”
- The Cargo Manifest data for unloading and/or loading is included.
- The system must validate that referred container identifiers comply with the ISO 6346:2022 standard.
- Crew information (name, citizen ID, nationality) may be requested when necessary for compliance with security protocols.
- Vessel Visit Notifications may remain in an "in progress" status until completed.
- Once completed, the representative must change the status to "submitted" to request approval.

**Clarifications:**

- A Vessel Visit may include one, both, or no manifests depending on the nature of the visit.
- Dangerous cargo requires identification of crew safety officers.

---

### 1.3. Acceptance Criteria

- The notification must include vessel ID, ETA, ETD, cargo manifest(s), and crew details if required.
- Container IDs must be validated against ISO 6346:2022.
- The notification must support both loading and unloading manifests.
- The initial status is "in progress" and must be explicitly changed to "submitted" by the representative.
- The operation must respect the system’s layered architecture principles.

---

### 1.4. Found Dependencies

- US_2.2.9 – Update Vessel Visit Notification
- US_2.2.10 – View Vessel Visit Status
- US_2.2.7 – Approve Vessel Visit Notification
- US_2.2.2 – Register Vessel
- US_2.2.6 – Manage Shipping Agent Representatives

---

### 1.5. Other Relevant Remarks

- The submission process follows the Clean Architecture dependency rule (UI → Application → Domain).
- Cargo manifest validation and crew data handling must comply with GDPR and port safety regulations.

---