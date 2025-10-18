# US_2.2.9 – Change or Complete a Vessel Visit Notification

## 1. Requirements

### 1.1. User Story

**As a Shipping Agent Representative**,  
I want to change or complete a Vessel Visit Notification while it is still in progress,  
so that I can correct errors or withdraw requests if necessary.

---

### 1.2. Customer Specifications and Clarifications

**From the specification document:**

- “As a Shipping Agent Representative, I want to change / complete a Vessel Visit Notification while it is still in
  progress, so that I can correct errors or withdraw requests if necessary.”
- Status can be maintained as “in progress” or changed to “submitted / approval pending” by the representative.

**Clarifications:**
>
> > **Question:** Boa tarde, Reparei que na US 2.2.8 refere, no acceptance criteria, que informação pode ser adicionada no futuro. Apenas deu o exemplo de adicionar informação da carga. Informação de tripulantes ou outros dados podem ser alterados ou adicionados mais tarde? Ou apenas certas informações podem ser adicionadas/alteradas. Cumprimentos, Grupo 03
>
> > **Answer:** 
> >* [PT] Enquanto o estado do Vessel Visit Notification for "in progress" (US 2.2.8 e US 2.2.9) todos os seus dados podem ser alterados/adicionados. Depois de ser submetido, já não pode ser alterado pelo Shipping Agent Representative.
> >* [EN] While the Vessel Visit Notification status is "in progress" (US 2.2.8 and US 2.2.9), all its data can be changed/added.Once submitted, it can no longer be changed by the Shipping Agent Representative.
>
> > **Question:**  In the US, the term "withdraw request" is often used. Could you clarify what this action consists of? Specifically:When an order is withdrawn, can it later be restored, or does it disappear permanently? If the status of a notification is "submitted", is it possible to withdraw that request?
>
> > **Answer:** Under the US 2.2.9, the mention to "withdraw request" refers to the ability of the Shipping Agent Representative to mark a given Vessel Visit Notification as having no intention to complete it til the point of submitting it for approval. As so, (s)he does not see that Notification as being "in progress" any more. However, the Notification should not be deleted since, occasionally, (s)he may change her/his mind a resume it from there. After being submitted, the Shipping Agent Representative cannot change the Notification.
>
> > > **Question:**  Should the shipping agent representative who requests to modify or remove a Vessel Visit Notification be allowed to change only the notifications they created, or any notification in the system, regardless of who created it?
>
> > **Answer:** Most of the time, Shipping Agent Representative work on the Vessel Visit Notifications created by themselves.
However, it may be possible to work on Vessel Visit Notifications submitted by other representatives working for the same shipping agent organization.
>


---

### 1.3. Acceptance Criteria

- Representatives can modify details while the status is “in progress.”
- When the representative submits the notification, the status changes to “submitted / approval pending.”
- Once submitted, the notification cannot be edited but can be withdrawn.
- All modifications must be recorded with timestamps and user identification.

---

### 1.4. Found Dependencies

- US_2.2.8 – Create Vessel Visit Notification
- Authentication / Authorization module


---

### 1.5. Other Relevant Remarks

- The notification’s lifecycle is managed by status transitions: **In Progress → Submitted / Approval Pending →
  Approved / Rejected**.
- Must comply with the dependency inversion and separation of concerns principles described in *Layered Architecture
  Patterns v8*.

---

