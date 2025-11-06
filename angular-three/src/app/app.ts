import { Component } from "@angular/core";
import { CommonModule } from "@angular/common";
import { TranslationService } from "./translation.service";
import {AuthService} from "./auth.service";
@Component({
  selector: "app-root",
  standalone: true,
  imports: [CommonModule],
  templateUrl: "./app.html",
  styleUrls: ["./app.scss"]
})
export class App {
  currentLang = 'en';

  // menu item objects: label = translation key, anchor = section id
  menuItems = [
    { label: 'menuHome', anchor: 'home' },
    { label: 'menuAbout', anchor: 'about' },
    { label: 'menuOperations', anchor: 'operations' },
    { label: 'menuSustainability', anchor: 'sustainability' },
    { label: 'menuNews', anchor: 'news' },
    { label: 'menuContact', anchor: 'contact' }
  ];

  constructor(private translation: TranslationService, private authService : AuthService) {
    this.currentLang = this.translation.getLang();
    if(typeof(window) != 'undefined') {
      this.authService.initOAuth();
    }
  }

  t(key: string): string {
    return this.translation.translate(key);
  }

  switchLanguage(lang: string): void {
    this.translation.setLanguage(lang);
    this.currentLang = this.translation.getLang();
  }
  login() {
    this.authService.login();
  }

  logout() {
    this.authService.logout();
  }

  get isLoggedIn(): boolean {
    return this.authService.isLoggedIn;
  }

  get userName(): string | null {
    const claims = this.authService['oauthService']?.getIdentityClaims() as any;
    return claims ? claims.name : null;
  }
}
