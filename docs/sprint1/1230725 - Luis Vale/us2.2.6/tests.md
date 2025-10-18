
## 4. Tests

### 4.1. Unit Tests
- **Test Create Valid Representative:**
    - Validate that a representative can be created for a valid organization.
    - Ensure representative status defaults to “Active”.

- **Test Update Representative Contact Info:**
    - Update representative email and phone number.
    - Verify changes persist in the database and notification is sent.

- **Test Deactivate Representative:**
    - Deactivate a representative and verify status changes to “Inactive”.
    - Ensure deactivated representative cannot authenticate.

- **Test Reactivate Representative:**
    - Reactivate a previously deactivated representative.
    - Confirm status changes to “Active” and access is restored.

- **Test Duplicate Citizen ID or Email:**
    - Attempt to create a representative with an existing Citizen ID or Email within the same organization.
    - System should reject the request with a **409 Conflict**.

- **Test Association to Organization:**
    - Ensure representative cannot exist without a valid organization ID.
    - System should validate organization existence before creation.
    - 
### 4.2. Functional Test

- **Test Create Representative via UI:**
    - Officer opens organization details and selects “Add Representative”.
    - Officer enters valid data and submits.
    - System displays success message and representative appears in list.

- **Test Edit Representative via UI:**
    - Officer edits a representative’s email and phone.
    - System confirms update and shows notification sent message.

- **Test Deactivate and Reactivate Representative:**
    - Officer deactivates a representative; system marks status as “Inactive”.
    - Officer reactivates the same representative; system updates status to “Active”.

- **Test Duplicate Data Validation:**
    - Officer attempts to register another representative with the same Citizen ID.
    - System rejects operation and displays duplicate data message.
