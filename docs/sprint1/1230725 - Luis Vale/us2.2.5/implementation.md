
# 5. User Story Implementation Report

---

## 5.1. US_2.2.5 – Register Shipping Agent Organizations

### 5.2. Description

As a Port Authority Officer, I want to register new shipping agent organizations so that they can operate within the
port’s digital system.

### 5.3. Implementation Details

- **User Interaction & Flow Control**  
  A new UI (`RegisterShippingAgentUI`) was developed to allow Port Authority Officers to input organization and
  representative details.

- **Business Logic Coordination**  
  The `RegisterShippingAgentController` manages the registration process and delegates validation and persistence to the
  application service.

- **Domain Entity Construction**  
  The `ShippingAgentOrganization` entity encapsulates organization data and validation logic, while `Representative`
  stores contact and identification details.

- **Persistence**  
  The `IShippingAgentRepository` interface and its implementation (`ShippingAgentRepositorySQL`) persist organization
  and representative data in the database.

- **Notifications**  
  The `EmailNotificationService` sends confirmation messages to representatives after successful registration.

---
