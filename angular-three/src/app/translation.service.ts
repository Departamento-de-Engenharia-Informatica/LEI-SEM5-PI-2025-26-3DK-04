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

      login: "Login",

      heroTitle: "The port of Amsterdam",
      heroSubtitle: "On our way to a climate-neutral port",

      aboutTitle: "About the Project",
      aboutText: "This page contains the project description and group information.",

      members: [
        { name: "Student One", number: "Number 1" },
        { name: "Student Two", number: "Number 2" },
        { name: "Student Three", number: "Number 3" },
        { name: "Student Four", number: "Number 4" },
        { name: "Student Five", number: "Number 5" }
      ]
    },

    pt: {
      appTitle: "Projeto",
      appSubtitle: "Portal do Trabalho de Grupo",

      menuHome: "Início",
      menuAbout: "Sobre",
      menuGroupMembers: "Membros do Grupo",

      login: "Entrar",

      heroTitle: "O porto de Amesterdão",
      heroSubtitle: "A caminho de um porto neutro em carbono",

      aboutTitle: "Sobre o Projeto",
      aboutText: "Esta página contém a descrição do projeto e informações do grupo.",

      members: [
        { name: "Aluno Um", number: "Número 1" },
        { name: "Aluno Dois", number: "Número 2" },
        { name: "Aluno Três", number: "Número 3" },
        { name: "Aluno Quatro", number: "Número 4" },
        { name: "Aluno Cinco", number: "Número 5" }
      ]
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
    return this.translations[this.currentLang]?.[key] ?? key;
  }
}
