# 5. User Story Implementation Report

---

## 5.1. US_2.2.8 – Submit Vessel Visit Notification

### 5.2. Description

As a Shipping Agent Representative, I want to create and submit a Vessel Visit Notification so that vessel berthing and subsequent (un)loading operations at the port can be properly scheduled and coordinated by the Port Authority.

---

### 5.3. Implementation Details

- **User Interaction & Flow Control**  
  A new UI (`SubmitVesselVisitUI`) was developed to allow Shipping Agent Representatives to create and submit Vessel Visit Notifications. The interface guides users through entering vessel details, cargo information (including hazardous materials), and crew lists, ensuring all required data is provided before submission.

- **Business Logic Coordination**  
  The `SubmitVesselVisitController` orchestrates the submission workflow. It validates user input via the `SubmitVesselVisitService`, ensures compliance with business rules (such as mandatory Safety Officer for hazardous cargo), and manages transitions between the “In Progress” and “Submitted” statuses.

- **Domain Entity Construction**  
  The `VesselVisitNotification` entity encapsulates all relevant visit data — including vessel information, cargo manifests, crew lists, and submission status. It enforces validation rules (e.g., ISO 6346 container ID format, crew completeness) and supports controlled state transitions.

- **Persistence**  
  The `IVesselVisitRepository` interface and its implementation (`VesselVisitRepositorySQL`) handle persistence of vessel visit notifications. This includes saving all related cargo and crew data in a normalized database structure to ensure consistency and retrievability.

- **Validation & Error Handling**  
  The system performs multiple validation checks:
    - Ensures all required fields (vessel name, IMO number, cargo list, crew list) are completed.
    - Validates ISO 6346 container ID format for all containers.
    - Requires the presence of a Safety Officer when hazardous cargo is declared.  
      In case of validation failures, the system prevents submission and provides clear, user-friendly error messages.

- **Authorization & Security**  
  Role-based access control ensures that only authenticated users with the “Shipping Agent Representative” role can create and submit Vessel Visit Notifications. Unauthorized users are restricted from accessing or modifying submission data.

- **Status Management**  
  The submission process automatically updates the notification’s status:
    - “In Progress” while data is being entered or edited.
    - “Submitted” once all required fields are validated and the form is confirmed by the user.  
      These transitions are handled at the domain level to ensure data integrity.

- **Data Consistency & Integrity**  
  All submission operations are executed within transactional boundaries. In case of validation or persistence failure, the operation is rolled back to maintain database integrity and prevent incomplete records.

---
