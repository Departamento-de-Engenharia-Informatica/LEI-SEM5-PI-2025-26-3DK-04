import { Injectable } from "@angular/core";

@Injectable({
  providedIn: "root"
})
export class TranslationService {

  private currentLang = 'en';

  private translations: any = {

    en: {
      appTitle: "Project",
      appSubtitle: "Group Work Portal",

      menuHome: "Home",
      menuAbout: "About",
      menuGroupMembers: "Group Members",
      menuFunctionalities: "Functionalities",

      userInfo: "User Info",

      hello: "Hello",
      login: "Login",
      logout: "Logout",

      heroTitle: "The port of Amsterdam",
      heroSubtitle: "On our way to a climate-neutral port",

      aboutTitle: "About the Project",
      aboutText: "This page contains the project description and group information.",

      groupMembersTitle: "Group Members",

      members: [
        { name: "Student One", number: "Number 1" },
        { name: "Student Two", number: "Number 2" },
        { name: "Student Three", number: "Number 3" },
        { name: "Student Four", number: "Number 4" },
        { name: "Student Five", number: "Number 5" }
      ],

      userSection: {
        title: "User",
        name: "Name",
        email: "Email",
        role: "Role",
        status: "Status",
        picture: "Picture"
      },

      adminUI: {
        title: "Admin Panel",
        subtitle: "Select an administration action:",
        cards: {
          manageUsers: "Manage Users",
          manageUsersDesc: "Edit roles and permissions",

          logs: "System Logs",
          logsDesc: "View activity logs",

          reports: "Reports",
          reportsDesc: "Access system reports",

          vesselScheduling: "Vessel Scheduling",
          vesselSchedulingDesc: "Calculate optimal vessel scheduling to minimize delays",

          manageDocks: "Manage Docks",
          manageDocksDesc: "Add, edit, or remove docks",

          manageStorageAreas: "Manage Storage Areas",
          manageStorageAreasDesc: "Create, edit or remove storage areas"
        },
        manageUsers: {
          title: "User Management",
          checkTitle: "Check if a user exists",
          emailPlaceholder: "User Email",
          checkButton: "Check",
          createTitle: "Create New User",
          namePlaceholder: "Name",
          picturePlaceholder: "Picture URL",
          createButton: "Create User",
          updateTitle: "Update User Role",
          updateButton: "Update Role",
          userFound: "User found",
          backButton: "Back",
          emailEmpty: "Please enter an email",
          selfModifyError: "You cannot modify yourself",
          serverError: "Server error, try again later",
          createSuccess: "User created successfully",
          createError: "Error creating user",
          updateSuccess: "User role updated successfully",
          updateError: "Error updating user role"
        },
        manageDocks: {
          title: "Manage Docks",
          listTitle: "Docks List",
          refresh: "Refresh",
          noDocks: "No docks available.",

          createTitle: "Create New Dock",
          editTitle: "Edit Dock",
          name: "Name",
          namePlaceholder: "Dock name",
          location: "Location",
          locationPlaceholder: "Location or coordinates",
          length: "Length (m)",
          depth: "Depth (m)",
          maxDraft: "Max Draft (m)",
          coordinates: "Coordinates (e.g., 0,0)",
          vesselTypes: "Vessel Types",
          actions: "Actions",        // inglês
          create: "Create",
          saveChanges: "Save changes",
          cancel: "Cancel",
          edit: "Edit",
          delete: "Delete",
          confirmDelete: "Are you sure you want to delete this dock?",
          createdSuccess: "Dock created successfully.",
          updatedSuccess: "Dock updated successfully.",
          deletedSuccess: "Dock removed successfully.",

          validation: {
            requiredName: "Name is required.",
            requiredLocation: "Location is required.",
            requiredVesselType: "Vessel Type is required."
          }
        },
        manageStorageAreas: {
          title: "Manage Storage Areas",
          listTitle: "Storage Areas List",
          refresh: "Refresh",
          noStorageAreas: "No storage areas available.",
          code: "Code",
          designation: "Designation",
          type: "Type",
          location: "Location",
          capacity: "Capacity (TEUs)",
          actions: "Actions",
          edit: "Edit",
          delete: "Delete",
          createTitle: "Create New Storage Area",
          editTitle: "Edit Storage Area",
          coordinates: "Coordinates",
          initialDockAssignments: "Initial Dock Assignments",
          saveChanges: "Save changes",
          create: "Create",
          cancel: "Cancel",
          codePlaceholder: "Storage area code",
          designationPlaceholder: "Storage area designation",
          locationPlaceholder: "Location",
          capacityPlaceholder: "Maximum capacity in TEUs",
          dockAssignments: "Dock Assignments",
        },
        roles: {
          representative: "Representative",
          portAuthority: "Port Authority Officer",
          logistics: "Logistics Operator",
          projectManager: "Project Manager"
        }

      },

      representativeUI: {
        title: "Representative Dashboard",
        subtitle: "Overview of your assigned tasks",
        cards: {
          requests: "Handle Requests",
          requestsDesc: "Review pending client requests",

          info: "Company Info",
          infoDesc: "View registered company data"
        }
      },

      portAuthorityUI: {
        title: "Port Authority Dashboard",
        subtitle: "Port operational management",
        cards: {
          schedule: "Vessel Scheduling",
          scheduleDesc: "Approve or deny docking schedules",

          inspection: "Port Inspections",
          inspectionDesc: "Monitor port inspection data"
        }
      },

      logisticsUI: {
        title: "Logistics Dashboard",
        subtitle: "Cargo and transport management",
        cards: {
          cargo: "Manage Cargo",
          cargoDesc: "Track cargo assignments",

          fleet: "Fleet Info",
          fleetDesc: "Review transport vehicles"
        }
      },

      projectManagerUI: {
        title: "Project Manager Dashboard",
        subtitle: "Team and planning tools",
        cards: {
          tasks: "Team Tasks",
          tasksDesc: "Assign and manage internal tasks",

          progress: "Project Progress",
          progressDesc: "Review timeline progress",

          dockView: "Dock View",
          dockViewDesc: "View and manage dock operations in 3D visualization"
        }
      },

      vesselScheduling: {
        title: "Vessel Scheduling - Shortest Delay",
        subtitle: "Calculate optimal vessel scheduling to minimize delays",
        targetDate: "Target Date",
        calculateSchedule: "Calculate Schedule",
        calculating: "Calculating...",
        clearResults: "Clear Results",
        error: "Error",
        summaryCards: {
          targetDate: "Target Date",
          totalVessels: "Total Vessels",
          totalDelay: "Total Delay"
        },
        timeUnits: "time units",
        table: {
          title: "Vessel Schedule Details",
          status: "Status",
          vessel: "Vessel",
          arrival: "Arrival",
          departure: "Departure",
          startTime: "Start Time",
          endTime: "End Time",
          duration: "Duration",
          delay: "Delay"
        },
        timeline: {
          title: "Visual Timeline"
        }
      },

      accessDenied: {
        title: "Access Denied",
        message: "You do not have permission to view this page.",
        backHome: "Return to Home"
      },
      activation: {
        pending: "Activating...",
        success: "Activation Successful",
        error: "Activation Failed",
        successMessage: "Account activated successfully.",
        errorMessage: "Activation failed or token invalid.",
        invalidMessage: "Invalid activation request.",
        backHome: "Back to Home",
        afterActivation: "After activation you can try to log in with the account."
      },

    },


    pt: {
      appTitle: "Projeto",
      appSubtitle: "Portal do Trabalho de Grupo",

      menuHome: "Início",
      menuAbout: "Sobre",
      menuGroupMembers: "Membros do Grupo",
      menuFunctionalities: "Funcionalidades",

      userInfo: "Informação do Utilizador",
      hello: "Olá",
      login: "Entrar",
      logout: "Sair",

      heroTitle: "O porto de Amesterdão",
      heroSubtitle: "A caminho de um porto neutro em carbono",

      aboutTitle: "Sobre o Projeto",
      aboutText: "Esta página contém a descrição do projeto e informações do grupo.",

      groupMembersTitle: "Membros do Grupo",

      members: [
        { name: "Aluno Um", number: "Número 1" },
        { name: "Aluno Dois", number: "Número 2" },
        { name: "Aluno Três", number: "Número 3" },
        { name: "Aluno Quatro", number: "Número 4" },
        { name: "Aluno Cinco", number: "Número 5" }
      ],

      userSection: {
        title: "Utilizador",
        name: "Nome",
        email: "Email",
        role: "Função",
        status: "Estado",
        picture: "Foto"
      },

      adminUI: {
        title: "Painel de Administração",
        subtitle: "Escolha uma ação administrativa:",
        cards: {
          manageUsers: "Gerir Utilizadores",
          manageUsersDesc: "Editar funções e permissões",

          logs: "Registos do Sistema",
          logsDesc: "Ver registos de atividade",

          reports: "Relatórios",
          reportsDesc: "Aceder a relatórios do sistema",

          vesselScheduling: "Agendamento de Navios",
          vesselSchedulingDesc: "Calcular agendamento ótimo de navios para minimizar atrasos",

          manageDocks: "Gerir Docas",
          manageDocksDesc: "Adicionar, editar ou remover docas",

          manageStorageAreas: "Gerir Áreas de Armazenamento",
          manageStorageAreasDesc: "Criar, editar ou remover áreas de armazenamento"
        },
        manageUsers: {
          title: "Gestão de Utilizadores",
          checkTitle: "Verificar se o utilizador existe",
          emailPlaceholder: "Email do Utilizador",
          checkButton: "Verificar",
          createTitle: "Criar Novo Utilizador",
          namePlaceholder: "Nome",
          picturePlaceholder: "URL da Imagem",
          createButton: "Criar Utilizador",
          updateTitle: "Atualizar Função do Utilizador",
          updateButton: "Atualizar Função",
          userFound: "Utilizador encontrado",
          backButton: "Voltar",
          emailEmpty: "Por favor insira um email",
          selfModifyError: "Não pode modificar a si próprio",
          serverError: "Erro do servidor, tente novamente mais tarde",
          createSuccess: "Utilizador criado com sucesso",
          createError: "Erro ao criar o utilizador",
          updateSuccess: "Função do utilizador atualizada com sucesso",
          updateError: "Erro ao atualizar função do utilizador"
        },
        manageDocks: {
          title: "Gerir Docas",
          listTitle: "Lista de Docas",
          refresh: "Atualizar",
          noDocks: "Não existem docas.",

          createTitle: "Criar Nova Doca",
          editTitle: "Editar Doca",
          name: "Nome",
          namePlaceholder: "Nome da doca",
          location: "Localização",
          locationPlaceholder: "Localização ou coordenadas",
          length: "Comprimento (m)",
          depth: "Profundidade (m)",
          maxDraft: "Calado Máximo (m)",
          coordinates: "Coordenadas (ex.: 0,0)",
          vesselTypes: "Tipos de Embarcação",
          actions: "Ações",          // português
          create: "Criar",
          saveChanges: "Guardar alterações",
          cancel: "Cancelar",
          edit: "Editar",
          delete: "Remover",
          confirmDelete: "Tem a certeza que pretende remover esta doca?",
          createdSuccess: "Doca criada com sucesso.",
          updatedSuccess: "Doca atualizada com sucesso.",
          deletedSuccess: "Doca removida com sucesso.",

          validation: {
            requiredName: "O nome é obrigatório.",
            requiredLocation: "A localização é obrigatória.",
            requiredVesselType: "O tipo de embarcação é obrigatório."
          }
        },
// Inside the "pt" section
        manageStorageAreas: {
          title: "Gerir Áreas de Armazenamento",
          listTitle: "Lista de Áreas de Armazenamento",
          refresh: "Atualizar",
          noStorageAreas: "Não existem áreas de armazenamento.",
          code: "Código",
          designation: "Designação",
          type: "Tipo",
          location: "Localização",
          capacity: "Capacidade (TEUs)",
          actions: "Ações",
          edit: "Editar",
          delete: "Remover",
          createTitle: "Criar Nova Área de Armazenamento",
          editTitle: "Editar Área de Armazenamento",
          coordinates: "Coordenadas",
          initialDockAssignments: "Atribuições Iniciais de Docas",
          saveChanges: "Guardar alterações",
          create: "Criar",
          cancel: "Cancelar",
          codePlaceholder: "Código da área de armazenamento",
          designationPlaceholder: "Designação da área de armazenamento",
          locationPlaceholder: "Localização",
          capacityPlaceholder: "Capacidade máxima em TEUs",
          dockAssignments: "Atribuições de Docas",
        },
        roles: {
          representative: "Representante",
          portAuthority: "Oficial da Autoridade Portuária",
          logistics: "Operador Logístico",
          projectManager: "Gestor de Projeto"
        }
      },

      representativeUI: {
        title: "Painel de Representante",
        subtitle: "Resumo das suas tarefas atribuídas",
        cards: {
          requests: "Gerir Pedidos",
          requestsDesc: "Rever pedidos pendentes",

          info: "Informação da Empresa",
          infoDesc: "Ver dados registados"
        }
      },

      portAuthorityUI: {
        title: "Painel da Autoridade Portuária",
        subtitle: "Gestão operacional do porto",
        cards: {
          schedule: "Agendamento de Navios",
          scheduleDesc: "Aprovar ou rejeitar horários de docagem",

          inspection: "Inspeções do Porto",
          inspectionDesc: "Monitorizar dados de inspeções"
        }
      },

      logisticsUI: {
        title: "Painel de Logística",
        subtitle: "Gestão de carga e transporte",
        cards: {
          cargo: "Gerir Carga",
          cargoDesc: "Acompanhar cargas atribuídas",

          fleet: "Informação da Frota",
          fleetDesc: "Ver veículos de transporte"
        }
      },

      projectManagerUI: {
        title: "Painel de Gestor de Projeto",
        subtitle: "Ferramentas de equipa e planeamento",
        cards: {
          tasks: "Tarefas da Equipa",
          tasksDesc: "Atribuir e gerir tarefas internas",

          progress: "Progresso do Projeto",
          progressDesc: "Rever evolução da timeline",

          dockView: "Visualização do Porto",
          dockViewDesc: "Visualize e gerencie operações portuárias em 3D"
        }
      },

      vesselScheduling: {
        title: "Agendamento de Navios - Menor Atraso",
        subtitle: "Calcular agendamento ótimo de navios para minimizar atrasos",
        targetDate: "Data Alvo",
        calculateSchedule: "Calcular Agendamento",
        calculating: "A calcular...",
        clearResults: "Limpar Resultados",
        error: "Erro",
        summaryCards: {
          targetDate: "Data Alvo",
          totalVessels: "Total de Navios",
          totalDelay: "Atraso Total"
        },
        timeUnits: "unidades de tempo",
        table: {
          title: "Detalhes do Agendamento de Navios",
          status: "Estado",
          vessel: "Navio",
          arrival: "Chegada",
          departure: "Partida",
          startTime: "Início",
          endTime: "Fim",
          duration: "Duração",
          delay: "Atraso"
        },
        timeline: {
          title: "Linha do Tempo Visual"
        }
      },

      accessDenied: {
        title: "Acesso Negado",
        message: "Não tem permissão para ver esta página.",
        backHome: "Voltar ao Início"
      },
      activation: {
        pending: "A ativar...",
        success: "Ativação bem-sucedida",
        error: "Falha na ativação",
        successMessage: "Conta ativada com sucesso.",
        errorMessage: "Falha na ativação ou token inválido.",
        invalidMessage: "Pedido de ativação inválido.",
        backHome: "Voltar ao Início",
        afterActivation: "Após a ativação, pode tentar iniciar sessão com a conta."
      },


    }
  };


  setLanguage(lang: string) {
    if (this.translations[lang]) {
      this.currentLang = lang;
    }
  }

  getLang(): string {
    return this.currentLang;
  }

  translate(key: string): any {
    const path = key.split('.');
    let current = this.translations[this.currentLang];

    for (const segment of path) {
      if (current[segment] === undefined) {
        return key; // fallback (mostra a key no HTML)
      }
      current = current[segment];
    }

    return current;
  }

}
