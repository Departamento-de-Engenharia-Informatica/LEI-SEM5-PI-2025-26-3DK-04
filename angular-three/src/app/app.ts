
import { Component, OnInit, OnDestroy } from "@angular/core";
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
export class App implements OnInit, OnDestroy {
  currentLang = "en";
  currentAnchor = "home";

  menuItems = [
    { label: "menuHome", anchor: "home" },
    { label: "menuAbout", anchor: "about" },
    { label: "menuGroupMembers", anchor: "GroupMembers" }
  ];

  constructor(private translation: TranslationService, private authService : AuthService) {
    this.currentLang = this.translation.getLang();
    if(typeof(window) != 'undefined') {
      this.authService.initOAuth();
    }
  }
  private scrollHandler = () => this.onWindowScroll();

  ngOnInit(): void {
    // set initial active anchor based on scroll position (in case user landed in middle)
    setTimeout(() => this.onWindowScroll(), 0);
    window.addEventListener("scroll", this.scrollHandler, { passive: true });
  }

  ngOnDestroy(): void {
    window.removeEventListener("scroll", this.scrollHandler);
  }

  t(key: string): any {
    return this.translation.translate(key);
  }

  // called when clicking EN/PT links
  onLangClick(ev: Event, lang: string): void {
    ev.preventDefault();
    if (lang === this.currentLang) return;
    this.switchLanguage(lang);
  }

  switchLanguage(lang: string): void {
    this.translation.setLanguage(lang);
    this.currentLang = this.translation.getLang();
    // no page reload — template will reflect the translation service content
  }

  // handle navigation clicks to anchors in a SPA way (no full reload)
  onNavClick(ev: Event, anchor: string): void {
    ev.preventDefault();
    this.scrollToAnchor(anchor);
    this.currentAnchor = anchor;
    // update URL hash without causing jump or reload
    try {
      history.replaceState(null, "", `#${anchor}`);
    } catch (e) {
      // ignore if not supported
    }
  }

  scrollToAnchor(anchor: string): void {
    const el = document.getElementById(anchor);
    if (el) {
      const top = el.getBoundingClientRect().top + window.scrollY - 72; // offset for fixed header
      window.scrollTo({
        top,
        behavior: "smooth"
      });
    } else {
      // fallback: change location hash (no reload)
      try {
        location.hash = `#${anchor}`;
      } catch (e) {}
    }
  }

  // update active nav item while user scrolls
  onWindowScroll(): void {
    const offsets = this.menuItems.map((it) => {
      const el = document.getElementById(it.anchor);
      if (!el) return { anchor: it.anchor, top: Infinity };
      const rect = el.getBoundingClientRect();
      const top = rect.top + window.scrollY - 100; // tolerance
      return { anchor: it.anchor, top };
    });

    // find the section with top <= current scroll position, closest to top
    const scrollPos = window.scrollY;
    let current = this.currentAnchor;
    for (let i = offsets.length - 1; i >= 0; i--) {
      if (scrollPos >= offsets[i].top) {
        current = offsets[i].anchor;
        break;
      }
    }

    // if we're near the top (before first section), set to first anchor
    if (scrollPos < (offsets[0]?.top ?? 0)) {
      current = this.menuItems[0].anchor;
    }

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
