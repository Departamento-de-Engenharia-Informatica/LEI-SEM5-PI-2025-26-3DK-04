
## 4. Tests

### 4.1. Unit Tests

- Create a representative with valid data.
- Attempt to create representative with duplicate email → expect failure.
- Deactivate representative → verify status = Inactive.
- Update contact info and verify persistence.

### 4.2. Functional Test

- Officer creates representative → system confirms creation and sends email.
- Officer deactivates representative → system confirms and blocks access.

---

