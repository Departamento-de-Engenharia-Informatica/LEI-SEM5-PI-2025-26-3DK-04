# US 2.2.7 - Review Pending Vessel Visit Notifications

## ✅ Implementação Completa

### 📁 Ficheiros Criados/Modificados:

#### Domain Layer
- ✅ `Domain/Vessels/VesselVisitNotification.cs` - Entidade com métodos Approve() e Reject()
- ✅ `Domain/Vessels/VesselVisitNotificationDto.cs` - DTO de resposta
- ✅ `Domain/Vessels/ApproveNotificationDto.cs` - DTO para aprovar
- ✅ `Domain/Vessels/RejectNotificationDto.cs` - DTO para rejeitar
- ✅ `Domain/Vessels/IVesselVisitNotificationRepository.cs` - Interface do repositório
- ✅ `Domain/Vessels/VesselVisitNotificationService.cs` - Lógica de aplicação

#### Infrastructure Layer
- ✅ `Infrastructure/Vessels/VesselVisitNotificationRepository.cs` - Implementação do repositório
- ✅ `Infrastructure/Vessels/VesselVisitNotificationEntityTypeConfiguration.cs` - Config EF
- ✅ `Infrastructure/DDDSample1DbContext.cs` - DbContext atualizado

#### Controllers
- ✅ `Controllers/VesselVisitNotificationsController.cs` - API REST endpoints

#### Configuration
- ✅ `Startup.cs` - Dependency Injection configurada

---

## 🚀 Como Testar

### 1️⃣ Executar a Aplicação

```bash
dotnet run
```

A aplicação estará disponível em: `https://localhost:5001` ou `http://localhost:5000`

Aceda ao Swagger: `https://localhost:5001/swagger`

---

### 2️⃣ Endpoints Disponíveis

#### **GET /api/VesselVisitNotifications/completed**
Lista todas as notificações completadas (prontas para review)

```http
GET https://localhost:5001/api/VesselVisitNotifications/completed
```

**Resposta 200 OK:**
```json
[
  {
    "id": "3fa85f64-5717-4562-b3fc-2c963f66afa6",
    "state": "Completed",
    "assignedDock": null,
    "rejectedReason": null,
    "decisionTimeStamp": null,
    "decisionOutcome": null,
    "officerId": null
  }
]
```

---

#### **GET /api/VesselVisitNotifications/{id}**
Busca uma notificação específica

```http
GET https://localhost:5001/api/VesselVisitNotifications/3fa85f64-5717-4562-b3fc-2c963f66afa6
```

**Resposta 200 OK:**
```json
{
  "id": "3fa85f64-5717-4562-b3fc-2c963f66afa6",
  "state": "Completed",
  "assignedDock": null,
  "rejectedReason": null,
  "decisionTimeStamp": null,
  "decisionOutcome": null,
  "officerId": null
}
```

**Resposta 404 Not Found:**
```json
{
  "message": "Notification with ID ... not found."
}
```

---

#### **PUT /api/VesselVisitNotifications/{id}/approve**
Aprova uma notificação e atribui dock

```http
PUT https://localhost:5001/api/VesselVisitNotifications/3fa85f64-5717-4562-b3fc-2c963f66afa6/approve
Content-Type: application/json

{
  "dockId": "DOCK-A1",
  "officerId": "officer@port.com"
}
```

**Resposta 200 OK:**
```json
{
  "id": "3fa85f64-5717-4562-b3fc-2c963f66afa6",
  "state": "Approved",
  "assignedDock": "DOCK-A1",
  "rejectedReason": null,
  "decisionTimeStamp": "2025-10-10T14:30:00Z",
  "decisionOutcome": "Approved",
  "officerId": "officer@port.com"
}
```

**Resposta 400 Bad Request (erro de validação):**
```json
{
  "message": "Only notifications marked as completed can be approved."
}
```

---

#### **PUT /api/VesselVisitNotifications/{id}/reject**
Rejeita uma notificação com motivo

```http
PUT https://localhost:5001/api/VesselVisitNotifications/3fa85f64-5717-4562-b3fc-2c963f66afa6/reject
Content-Type: application/json

{
  "reason": "Missing cargo documentation",
  "officerId": "officer@port.com"
}
```

**Resposta 200 OK:**
```json
{
  "id": "3fa85f64-5717-4562-b3fc-2c963f66afa6",
  "state": "Rejected",
  "assignedDock": null,
  "rejectedReason": "Missing cargo documentation",
  "decisionTimeStamp": "2025-10-10T14:35:00Z",
  "decisionOutcome": "Rejected",
  "officerId": "officer@port.com"
}
```

**Resposta 400 Bad Request:**
```json
{
  "message": "A rejection reason must be provided."
}
```

---

## ✅ Regras de Negócio Implementadas

### Aprovação (Approve):
- ✅ Só notificações **Completed** podem ser aprovadas
- ✅ **DockId** é obrigatório
- ✅ **OfficerId** é obrigatório
- ✅ Timestamp é gerado automaticamente
- ✅ Estado muda para **Approved**
- ✅ Decision outcome é registado

### Rejeição (Reject):
- ✅ Só notificações **Completed** podem ser rejeitadas
- ✅ **Reason** (motivo) é obrigatório
- ✅ **OfficerId** é obrigatório
- ✅ Timestamp é gerado automaticamente
- ✅ Estado muda para **Rejected**
- ✅ Decision outcome é registado
- ✅ Shipping agent pode depois atualizar (via US do teu colega)

### Auditoria:
- ✅ Todas as decisões têm **timestamp**
- ✅ Todas as decisões têm **officerId**
- ✅ Todas as decisões têm **decision outcome** (Approved/Rejected)

---

## 🧪 Cenários de Teste

### Teste 1: Aprovar Notificação Completada ✅
```
1. GET /completed → obter lista
2. PUT /{id}/approve com dockId válido
3. Verificar que estado = "Approved"
4. Verificar que assignedDock está preenchido
5. Verificar que decisionTimeStamp existe
```

### Teste 2: Rejeitar Notificação Completada ✅
```
1. GET /completed → obter lista
2. PUT /{id}/reject com reason válido
3. Verificar que estado = "Rejected"
4. Verificar que rejectedReason está preenchido
5. Verificar que decisionTimeStamp existe
```

### Teste 3: Tentar Aprovar Notificação Já Aprovada ❌
```
1. PUT /{id}/approve → sucesso
2. PUT /{id}/approve novamente → erro 400
3. Message: "Only notifications marked as completed can be approved."
```

### Teste 4: Tentar Aprovar Sem Dock ❌
```
1. PUT /{id}/approve com dockId vazio
2. Erro 400
3. Message: "A dock must be assigned when approving a notification."
```

### Teste 5: Tentar Rejeitar Sem Motivo ❌
```
1. PUT /{id}/reject com reason vazio
2. Erro 400
3. Message: "A rejection reason must be provided."
```

---

## 📊 Estados da Notificação

```
Draft → Pending → Completed → Approved
                           → Rejected → (pode voltar a Pending via ResetToPending)
```

**IMPORTANTE:** 
- O Port Authority Officer só revê notificações no estado **Completed**
- O shipping agent cria notificações (Draft/Pending)
- O shipping agent completa notificações (Completed)
- O officer aprova/rejeita notificações completadas

---

## 📝 Notas Importantes

1. **NotificationState enum** - Certifica-te que existe e tem os valores:
   - `Draft`
   - `Pending`
   - `Completed`
   - `Approved`
   - `Rejected`

2. **LoadingCargoMaterial e UnloadingCargoMaterial** - Estas classes devem existir

3. **VesselVisitNotificationID** - Já existe e herda de EntityId ✅

4. **Database** - Atualmente usa InMemory, dados perdem-se ao reiniciar

---

## 🎯 Critérios de Aceitação - COMPLETOS ✅

- ✅ Quando aprovada, officer atribui dock (temporarily)
- ✅ Quando rejeitada, officer fornece motivo
- ✅ Se rejeitada, shipping agent pode rever/atualizar (método ResetToPending disponível)
- ✅ Todas as decisões são logged com timestamp, officer ID e decision outcome

---

## 🔧 Próximos Passos (Para o Teu Colega)

O teu colega precisará implementar:

### US Create Notification:
- `POST /api/VesselVisitNotifications` - criar nova notificação (Draft)

### US Complete Notification:
- `PUT /api/VesselVisitNotifications/{id}/complete` - marcar como Completed (pronta para review)

### US Update Notification:
- `PUT /api/VesselVisitNotifications/{id}` - atualizar notificação rejeitada
- Usar método `ResetToPending()` para permitir re-submissão

---

**Implementação completa da US 2.2.7! 🚢✅**

**Para testar:**
```bash
dotnet run
```
Depois acede: `https://localhost:5001/swagger`
