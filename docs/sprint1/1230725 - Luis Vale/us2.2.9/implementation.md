# 5. User Story Implementation Report

## 5.1. US_2.2.9 – Change or Complete a Vessel Visit Notification

### 5.2. Description

As a Shipping Agent Representative, I want to edit or complete a Vessel Visit Notification while it’s still in progress,
so I can correct mistakes or withdraw requests when necessary.

### 5.3. Implementation Details

- **User Interaction & Flow Control:**  
  `ManageVesselVisitUI` allows editing, submission, and withdrawal.
- **Business Logic:**  
  `ManageVesselVisitService` enforces state rules and handles validations.
- **Persistence:**  
  `IVesselVisitRepository` updates and retrieves notification data.
- **Audit Logging:**  
  `AuditService` records all modifications, including timestamps and user identifiers.

---