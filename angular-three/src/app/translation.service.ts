import { Injectable } from "@angular/core";

@Injectable({
  providedIn: "root"
})
export class TranslationService {
  private currentLang = 'en';

  private translations: any = {
    en: {
      appTitle: "Port of Amsterdam",
      appSubtitle: "Gateway to global trade",

      menuHome: "Home",
      menuAbout: "About",
      menuOperations: "Operations",
      menuSustainability: "Sustainability",
      menuNews: "News",
      menuContact: "Contact",

      login: "Login",

      breadcrumbHome: "Home",
      breadcrumbLanding: "Landing",

      heroTitle: "Port of Amsterdam — Overview",
      heroSubtitle: "Europe’s fourth largest port, connecting trade, logistics and sustainability.",

      aboutTitle: "About the Port",
      aboutText: "Placeholder text about the Port of Amsterdam. Replace with the real content when available.",

      operationsTitle: "Operations & Services",
      op1Title: "Container Terminal",
      op1Text: "Placeholder description of container operations.",
      op2Title: "Bulk & Breakbulk",
      op2Text: "Placeholder description of bulk handling.",
      op3Title: "Logistics & Warehousing",
      op3Text: "Placeholder description of logistics services.",

      sustainTitle: "Sustainability",
      sustainText: "Placeholder overview of sustainability initiatives.",

      newsTitle: "News & Highlights",
      newsText: "Placeholder for latest news items about the port.",

      contactTitle: "Contact",
      contactText: "Placeholder contact information.",

      footerCopy: "© 2025 Port of Amsterdam - All rights reserved"
    },

    pt: {
      appTitle: "Porto de Amesterdão",
      appSubtitle: "Porta para o comércio global",

      menuHome: "Início",
      menuAbout: "Sobre",
      menuOperations: "Operações",
      menuSustainability: "Sustentabilidade",
      menuNews: "Novidades",
      menuContact: "Contacto",

      login: "Entrar",

      breadcrumbHome: "Início",
      breadcrumbLanding: "Página",

      heroTitle: "Porto de Amesterdão — Visão Geral",
      heroSubtitle: "O quarto maior porto da Europa, ligando comércio, logística e sustentabilidade.",

      aboutTitle: "Sobre o Porto",
      aboutText: "Texto placeholder sobre o Porto de Amesterdão. Substituir pelo conteúdo real quando disponível.",

      operationsTitle: "Operações & Serviços",
      op1Title: "Terminal de Contentores",
      op1Text: "Descrição placeholder das operações de contentores.",
      op2Title: "Carga a Granel & Geral",
      op2Text: "Descrição placeholder do tratamento de carga a granel.",
      op3Title: "Logística & Armazenagem",
      op3Text: "Descrição placeholder de serviços logísticos.",

      sustainTitle: "Sustentabilidade",
      sustainText: "Visão geral placeholder das iniciativas de sustentabilidade.",

      newsTitle: "Notícias & Destaques",
      newsText: "Placeholder para as últimas notícias sobre o porto.",

      contactTitle: "Contacto",
      contactText: "Informação de contacto placeholder.",

      footerCopy: "© 2025 Porto de Amesterdão - Todos os direitos reservados"
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

  translate(key: string): string {
    return this.translations[this.currentLang]?.[key] ?? key;
  }
}
