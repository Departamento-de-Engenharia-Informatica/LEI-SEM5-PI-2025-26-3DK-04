# US 2.2.9 - Edit or Submit Vessel Visit Notification

## Full Implementation

### Files Created/Modified

#### Domain Layer

* Domain/Vessels/VesselVisitNotification.cs — added methods Update(), Submit()
* Domain/Vessels/VesselVisitNotificationDto.cs
* Domain/Vessels/UpdateVesselVisitNotificationDto.cs
* Domain/Vessels/IVesselVisitNotificationRepository.cs
* Domain/Vessels/VesselVisitNotificationService.cs

#### Infrastructure Layer
* Infrastructure/Vessels/VesselVisitNotificationRepository.cs
* Infrastructure/Vessels/VesselVisitNotificationEntityTypeConfiguration.cs
* Infrastructure/DDDSample1DbContext.cs

#### Controllers
* Controllers/VesselVisitNotificationsController.cs — added update and submit endpoints

---

## How to Test

### Available Endpoints

#### **PUT /api/VesselVisitNotifications/{id}/update**
Update an in-progress notification

```http
PUT https://localhost:5001/api/VesselVisitNotifications/3fa85f64-5717-4562-b3fc-2c963f66afa6/update
Content-Type: application/json

{
  "vesselName": "Poseidon",
  "arrivalDate": "2025-11-01T10:00:00Z",
  "cargoDetails": "General goods",
  "status": "InProgress"
}
```

**Response 200 OK:**
```json
{
  "id": "3fa85f64-5717-4562-b3fc-2c963f66afa6",
  "status": "InProgress",
  "message": "Notification updated successfully."
}
```

---

#### **PUT /api/VesselVisitNotifications/{id}/submit**
Submit a notification for approval

```http
PUT https://localhost:5001/api/VesselVisitNotifications/3fa85f64-5717-4562-b3fc-2c963f66afa6/submit
```

**Response 200 OK:**
```json
{
  "id": "3fa85f64-5717-4562-b3fc-2c963f66afa6",
  "status": "Submitted",
  "submittedAt": "2025-10-17T15:00:00Z"
}
```

**Response 400 Bad Request:**
```json
{
  "message": "Only notifications in progress can be submitted."
}
```

---

## Implemented Business Rules

* Notifications with status “InProgress” can be edited
* Only “InProgress” notifications can be submitted
* Once submitted, they become read-only
* Edit history is maintained
* Cancellation is allowed before submission

## Tests

* Create notification → status InProgress
* Edit → status remains InProgress
* Submit → status changes to Submitted
* Edit after submission → 400 Bad Request