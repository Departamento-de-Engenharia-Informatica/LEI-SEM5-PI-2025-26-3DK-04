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
          reportsDesc: "Access system reports"
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
          backButton: "Back"
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
      }

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
          reportsDesc: "Aceder a relatórios do sistema"
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
          backButton: "Voltar"
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
      }

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
