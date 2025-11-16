import { Component, Inject, PLATFORM_ID } from '@angular/core';
import { TranslationService } from '../translation.service';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { RouterLink } from '@angular/router';

@Component({
  selector: 'app-port-authority-ui',
  standalone: true,
  templateUrl: './port-authority.ui.html',
  styleUrl: './port-authority.ui.scss',
  imports: [CommonModule, RouterLink]
})
export class PortAuthorityUI {
  currentLang = 'en';

  constructor(
    private translation: TranslationService,
    @Inject(PLATFORM_ID) private platformId: Object
  ) {
    const isBrowser = isPlatformBrowser(this.platformId);
    if (isBrowser) {
      const savedLang = localStorage.getItem('appLang');
      if (savedLang) {
        this.translation.setLanguage(savedLang);
        this.currentLang = savedLang;
      }
    }
  }

  t(key: string) {
    return this.translation.translate(key);
  }
}
