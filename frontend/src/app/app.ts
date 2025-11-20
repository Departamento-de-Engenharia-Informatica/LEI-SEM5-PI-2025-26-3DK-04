import { Component, OnInit, OnDestroy, Inject, PLATFORM_ID } from "@angular/core";
import { CommonModule, isPlatformBrowser } from "@angular/common";
import { RouterOutlet, RouterLink, RouterLinkActive, Router } from "@angular/router";
import { TranslationService } from "./translation.service";
import { AuthService } from "./auth.service";
import { CONFIG } from './config';

@Component({
  selector: "app-root",
  standalone: true,
  imports: [CommonModule, RouterOutlet, RouterLink, RouterLinkActive],
  templateUrl: "./app.html",
  styleUrls: ["./app.scss"]
})
export class App implements OnInit, OnDestroy {
  mobileMenuOpen = false;

  toggleMobileMenu() {
    this.mobileMenuOpen = !this.mobileMenuOpen;
  }

  closeMobileMenu() {
    this.mobileMenuOpen = false;
  }

  currentLang = "en";
  currentAnchor = "home";
  isBrowser: boolean;

  menuItems = [
    { label: "menuHome", anchor: "home" },
    { label: "menuAbout", anchor: "about" },
    { label: "menuGroupMembers", anchor: "GroupMembers" }
  ];

  constructor(
    private translation: TranslationService,
    private authService: AuthService,
    private router: Router, // ✅ adicionado para navegação sem refresh
    @Inject(PLATFORM_ID) private platformId: Object
  ) {
    this.isBrowser = isPlatformBrowser(this.platformId);

    // 🔹 Lê idioma guardado no browser (mantém idioma após reload)
    if (this.isBrowser) {
      const savedLang = localStorage.getItem('appLang');
      if (savedLang) {
        this.translation.setLanguage(savedLang);
        this.currentLang = savedLang;
      } else {
        this.currentLang = this.translation.getLang();
        localStorage.setItem('appLang', this.currentLang);
      }
    } else {
      this.currentLang = this.translation.getLang();
    }

    // 🔹 Só inicializa o OAuth no browser e deteta retorno do Google
    if (this.isBrowser) {
      this.handleGoogleCallback();
    }
  }

  private scrollHandler = () => this.onWindowScroll();

  ngOnInit(): void {
    if (!this.isBrowser) return;
    setTimeout(() => this.onWindowScroll(), 0);
    window.addEventListener("scroll", this.scrollHandler, { passive: true });
  }

  ngOnDestroy(): void {
    if (!this.isBrowser) return;
    window.removeEventListener("scroll", this.scrollHandler);
  }

  t(key: string): any {
    return this.translation.translate(key);
  }

  onLangClick(ev: Event, lang: string): void {
    ev.preventDefault();
    if (lang === this.currentLang) return;
    this.switchLanguage(lang);
  }

  switchLanguage(lang: string): void {
    this.translation.setLanguage(lang);
    this.currentLang = this.translation.getLang();
    if (this.isBrowser) {
      localStorage.setItem('appLang', this.currentLang);
    }
  }

  onNavClick(ev: Event, anchor: string): void {
    ev.preventDefault();
    this.scrollToAnchor(anchor);
    this.currentAnchor = anchor;

    if (this.isBrowser) {
      try {
        history.replaceState(null, "", `#${anchor}`);
      } catch {}
    }
  }

  scrollToAnchor(anchor: string): void {
    if (!this.isBrowser) return;
    const el = document.getElementById(anchor);
    if (el) {
      const top = el.getBoundingClientRect().top + window.scrollY - 72;
      window.scrollTo({ top, behavior: "smooth" });
    }
  }

  onWindowScroll(): void {
    if (!this.isBrowser) return;

    const scrollPos = window.scrollY;
    const offsets = this.menuItems.map((it) => {
      const el = document.getElementById(it.anchor);
      if (!el) return { anchor: it.anchor, top: Infinity };
      const rect = el.getBoundingClientRect();
      const top = rect.top + window.scrollY - 100;
      return { anchor: it.anchor, top };
    });

    let current = this.currentAnchor;
    for (let i = offsets.length - 1; i >= 0; i--) {
      if (scrollPos >= offsets[i].top) {
        current = offsets[i].anchor;
        break;
      }
    }

    if (scrollPos < (offsets[0]?.top ?? 0)) current = this.menuItems[0].anchor;
    this.currentAnchor = current;
  }

  login() {
    const clientId = '440853175141-2kp1hrvoe78b8pn597p8oc2b316dibq5.apps.googleusercontent.com';
    const redirectUri = `${CONFIG.frontendUrl}/`;
    const scope = 'openid profile email';
    const authUrl = `https://accounts.google.com/o/oauth2/v2/auth?response_type=code&client_id=${clientId}&redirect_uri=${redirectUri}&scope=${scope}&prompt=consent`;
    window.location.href = authUrl;
  }

  logout() {
    this.authService.logout();
  }

  get isLoggedIn(): boolean {
    return this.authService.isLoggedIn;
  }

  get userName(): string | null {
    return this.authService.userName;
  }
  get email(): string | null {
    return this.authService.email;
  }
  get role(): string | null {
    return this.authService.role;
  }
  get picture(): string | null {
    return this.authService.picture;
  }
  get status(): string | null {
    return this.authService.status;
  }

  get isAdmin(): boolean {
    return this.authService.role === 'Admin';
  }

  async handleGoogleCallback() {
    const params = new URLSearchParams(window.location.search);
    const code = params.get('code');
    console.log("meu parametro code é:", code);
    if (code) {
      try {
        const res = await fetch(`${CONFIG.authUrl}/google`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ code })
        });

        const data = await res.json();
        const idToken = data.idToken || '';

        const userRes = await fetch(`${CONFIG.authUrl}/google/user`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ idToken })
        });

        if (!userRes.ok) {
          const errorData = await userRes.json();
          alert(errorData.error);
          return;
        }

        const user = await userRes.json();
        const userName = this.parseUserNameFromIdToken(idToken);

        this.authService.setToken(idToken, user.name, user.email, user.picture, user.role, user.status);

        console.log('Login feito com sucesso:', user.name);
        console.log('Email', user.email);
        console.log('Role', user.role);
        console.log('Status', user.status);
        window.history.replaceState({}, document.title, '/');
      } catch (err) {
        console.error('Erro ao trocar code por token', err);
      }
    }
  }

  goToRoleUI() {
    const role = this.authService.role?.toLowerCase();

    if (!role) return;

    switch (role) {
      case "admin":
        this.router.navigate(["/admin"]);
        break;
      case "representative":
        this.router.navigate(["/representative"]);
        break;
      case "portauthorityofficer":
        this.router.navigate(["/port-officer"]);
        break;
      case "logisticsoperator":
        this.router.navigate(["/logistics"]);
        break;
      case "projectmanager":
        this.router.navigate(["/project-manager"]);
        break;
      default:
        this.router.navigate(["/access-denied"]);
        break;
    }
  }

  get rolePath(): string {
    const role = this.authService.role?.toLowerCase();
    switch (role) {
      case "admin": return "/admin";
      case "representative": return "/representative";
      case "portauthorityofficer": return "/port-officer";
      case "logisticsoperator": return "/logistics";
      case "projectmanager": return "/project-manager";
      default: return "/";
    }
  }

  // Função para extrair nome do id_token JWT
  parseUserNameFromIdToken(idToken: string): string | null {
    if (!idToken) return null;
    const payload = JSON.parse(atob(idToken.split('.')[1]));
    return payload.name || null;
  }
}
