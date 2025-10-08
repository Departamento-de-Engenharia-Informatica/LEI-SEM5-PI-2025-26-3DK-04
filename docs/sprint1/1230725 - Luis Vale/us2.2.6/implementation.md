# 5. User Story Implementation Report

## 5.1. US_2.2.6 – Register and Manage Representatives

### 5.2. Description

As a Port Authority Officer, I want to manage representatives of shipping agent organizations (create, update,
deactivate) so that only authorized users can operate on behalf of their organizations.

### 5.3. Implementation Details

- **User Interaction & Flow Control:**  
  `ManageRepresentativeUI` provides forms for CRUD operations.
- **Business Logic:**  
  `ManageRepresentativeService` validates data and coordinates repository and notification services.
- **Persistence:**  
  `IRepresentativeRepository` persists representative data; inactive users are not removed but flagged.
- **Notifications:**  
  `EmailNotificationService` informs representatives and their organizations about updates.

---