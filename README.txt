# LEI-SEM5-PI-2025-26-3DK-04

Descrição
--------

Este repositório contém a implementação de um serviço Web em .NET 8 (.NET Core) desenvolvido no âmbito do projeto do semestre 5 da LEI. O projeto segue uma arquitetura DDD (Domain-Driven Design) organizada em pastas como `Domain`, `Infraestructure`, `Controllers` e `Tests`.

Principais responsabilidades
- Gerir organizações, representantes e recursos do porto.
- Fornecer uma API REST com controladores para entidades como Docks, Organizations, Vessels, Qualifications, Representatives, StaffMembers, StorageAreas e VesselVisits.

Estrutura do repositório
- `Controllers/` - Endpoints HTTP do Web API (ex.: `OrganizationController.cs`, `VesselsController.cs`, ...).
- `Domain/` - Entidades, DTOs, serviços e interfaces de repositório do domínio.
- `Infraestructure/` - Implementação do DbContext (`DDDSample1DbContext.cs`), repositórios e UnitOfWork.
- `Migrations/` - Migrations do Entity Framework Core.
- `Tests/` - Testes automatizados (Application, Domain, Integration, System).

Como compilar e executar (PowerShell)

1. Restaurar dependências e compilar:

```powershell
dotnet build DDDNetCore.csproj
```

2. Executar a aplicação:

```powershell
dotnet run DDDNetCore.csproj
```

Abra o local host e escreva à frente do número da porta "/swagger", para poder testar os endpoints.

3. Executar os testes:

```powershell
dotnet test DDDNetCore.csproj
```

Isto executará os testes encontrados na pasta `Tests/` (Application, Domain, Integration, System).

API / Endpoints principais

Os controladores expostos encontram-se em `Controllers/`. Alguns dos controladores presentes:

- `DockController` — gerir cais e operações relacionadas.
- `OrganizationController` — gerir organizações.
- `PhysicalResourcesController` — gerir recursos físicos.
- `QualificationsController` — gerir qualificações.
- `RepresentativeController` — gerir representantes.
- `StaffMembersController` — gerir membros da equipa.
- `StorageAreaController` — gerir áreas de armazenamento.
- `VesselsController` — gerir navios.
- `VesselTypesController` — gerir tipos de navio.
- `VesselVisitNotificationsController` — gerir notificações de visitas de navio.

Contacto
-------
Para dúvidas sobre o projeto, contacte os autores listados no diretório `docs/`.