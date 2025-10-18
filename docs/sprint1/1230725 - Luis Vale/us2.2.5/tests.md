## 4. Tests

### 4.1. Unit Tests
- **Test Register Valid Organization:**
    - Verify that a new organization with valid data and at least one representative can be successfully registered.
    - Ensure the organization and representative records are persisted.
    - Confirm notification email is sent to representatives.

- **Test Register Without Representative:**
    - Attempt to register an organization without any representatives.
    - System should reject the operation and return a validation error.

- **Test Duplicate Organization Identifier:**
    - Attempt to register an organization using an identifier already in use.
    - System should return a conflict error (409).

- **Test Representative Data Validation:**
    - Verify system rejects invalid representative email or missing citizen ID.
    - Ensure proper error messages are returned for each invalid field.

- **Test Successful Persistence:**
    - Validate that a successfully created organization can be retrieved via repository query.
    - Ensure representative data is associated correctly.


### 4.2. Functional Tests
- **Test Organization Registration via UI:**
    - Officer logs in and navigates to "Register New Organization".
    - Officer fills in organization details and adds at least one representative.
    - System validates inputs and confirms creation.
    - Verify new organization appears in organization list with representative linked.

- **Test Error on Missing Fields:**
    - Officer attempts to register organization with missing tax number or address.
    - System displays validation errors and prevents submission.

- **Test Duplicate Organization Prevention:**
    - Officer tries to create organization with existing identifier.
    - System blocks and displays error message.


