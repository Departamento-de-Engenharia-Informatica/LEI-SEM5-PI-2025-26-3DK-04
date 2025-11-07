import { Component, OnInit, OnDestroy, Inject, PLATFORM_ID } from "@angular/core";
import { CommonModule, isPlatformBrowser } from "@angular/common";
import { TranslationService } from "./translation.service";
import { AuthService } from "./auth.service";

@Component({
  selector: "app-root",
  standalone: true,
  imports: [CommonModule],
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
    if (this.isBrowser) {
      this.authService.initOAuth();
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
