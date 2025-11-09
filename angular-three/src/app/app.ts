import { Component, OnInit, OnDestroy, Inject, PLATFORM_ID } from "@angular/core";
import { CommonModule, isPlatformBrowser } from "@angular/common";
import { RouterOutlet, RouterLink, RouterLinkActive } from "@angular/router";
import { TranslationService } from "./translation.service";
import { AuthService } from "./auth.service";

@Component({
  selector: "app-root",
  standalone: true,
  imports: [CommonModule, RouterOutlet, RouterLink, RouterLinkActive],
  templateUrl: "./app.html",
  styleUrls: ["./app.scss"]
})
export class App implements OnInit, OnDestroy {
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
    private authService : AuthService,
    @Inject(PLATFORM_ID) private platformId: Object
  ) {
    this.isBrowser = isPlatformBrowser(this.platformId);

    this.currentLang = this.translation.getLang();

    // Só inicializa OAuth no browser
    // Detecta o retorno do Google
    if (typeof window !== 'undefined') {
      this.handleGoogleCallback();
    }
  }

  private scrollHandler = () => this.onWindowScroll();

  ngOnInit(): void {
    if (!this.isBrowser) return;

    // set initial active anchor based on scroll position (in case user landed in middle)
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
    const redirectUri = 'http://localhost:4200/';
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
  async handleGoogleCallback() {
    const params = new URLSearchParams(window.location.search);
    const code = params.get('code');
    console.log("meu parametro code é:", code);
    if (code) {
      try {
        const res = await fetch('https://localhost:5001/auth/google', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ code })
        });

        const data = await res.json();
        const idToken = data.idToken || '';

        const userRes = await fetch('https://localhost:5001/auth/google/user', {
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

        this.authService.setToken(idToken, user.name,user.email,user.picture,user.role);

        console.log('Login feito com sucesso:', user.name);
        console.log('Email', user.email);
        console.log('Role', user.role);
        window.history.replaceState({}, document.title, '/');
      } catch (err) {
        console.error('Erro ao trocar code por token', err);
      }
    }

  }

// Função para extrair nome do id_token JWT
  parseUserNameFromIdToken(idToken: string): string | null {
    if (!idToken) return null;
    const payload = JSON.parse(atob(idToken.split('.')[1]));
    return payload.name || null;
  }
}
