# US_2.2.5 – Register Shipping Agent Organizations

### **As a Port Authority Officer**,

I want to register new shipping agent organizations,  
so that they can operate within the port’s digital system.

---

## 1. General Information

- **Main actor:** Port Authority Officer
- **Stakeholders:** Port Authority, Shipping Agents
- **Type:** Functional request

---

## 2. Description

The Port Authority Officer aims to register new shipping agent organizations so they can operate within the port’s
digital platform.

---

## 3. Objective

Ensure that only properly registered and validated organizations can access the system and represent shipowners or
vessels within the port platform.

---

## 4. Preconditions

- The user must be authenticated as a *Port Authority Officer*.
- The system must be connected to the organizations and users database.

---

## 5. Main Flow (Happy Path)

1. The Officer accesses the “Shipping Agent Registration” section.
2. Enters the organization’s information:
    - Identifier (ID)
    - Legal name
    - Alternative name (if applicable)
    - Address
    - Tax number (VAT)
3. Enters at least one representative:
    - Name
    - Citizen ID
    - Nationality
    - Email
    - Phone
4. The system validates the mandatory fields.
5. The system registers the organization and its representative(s).
6. A confirmation/decision email is sent.

---

## 6. Alternative Flows

- **A1:** Missing required fields → The system displays an error message.
- **A2:** Email already registered → The system asks for confirmation if it’s an existing organization.

---

## 7. Derived Functional Requirements

| ID  | Description                                                                    | Type       |
|-----|--------------------------------------------------------------------------------|------------|
| RF1 | The system must allow the creation of organization records.                    | Functional |
| RF2 | The system must ensure that each organization has at least one representative. | Functional |
| RF3 | The system must validate the uniqueness of email and tax number (VAT).         | Functional |
| RF4 | The system must send an email notification.                                    | Functional |

---

## 8. Non-Functional Requirements

| Category     | Description                                                                   |
|--------------|-------------------------------------------------------------------------------|
| Security     | Only users with the “Port Authority Officer” profile can access this feature. |
| Usability    | The interface must follow the application’s UX standards.                     |
| Performance  | Registration must be processed in under 3 seconds.                            |
| Architecture | Must comply with the principle of layered separation (Clean Architecture).    |

---

## 9. Analysis (Logical Modeling)

### **Conceptual Class Diagram**

**ShippingAgentOrganization**

- id, legalName, altName, address, taxNumber

**Representative**

- name, citizenId, nationality, email, phone

**Relationship:**  
`ShippingAgentOrganization 1..* — 1..* Representative`

**Related Use Cases**

- UC_2.2.6 – Edit Shipping Agent
- UC_2.2.7 – Approve Shipping Agent

---

## 10. Design (Architecture Layers)

Following the document *“Something About Layered Architecture Patterns v8”* and the principles of **Clean / Onion
Architecture**:

| Layer                            | Responsibility                         | Example Classes                                               |
|----------------------------------|----------------------------------------|---------------------------------------------------------------|
| **Domain**                       | Business rules and entities            | `ShippingAgent`, `Representative`, `IShippingAgentRepository` |
| **Application (Service Layer)**  | Orchestrates use cases and validations | `RegisterShippingAgentService`                                |
| **Infrastructure (Persistence)** | Implements repositories                | `ShippingAgentRepositorySQL`, `EmailNotificationService`      |
| **Interface (Presentation/UI)**  | Controllers, DTOs, REST endpoints      | `ShippingAgentController`, `ShippingAgentDTO`                 |

---


