import { Component, Inject, PLATFORM_ID } from '@angular/core';
import { TranslationService } from '../translation.service';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { RouterLink } from '@angular/router';

@Component({
  selector: 'app-project-manager-ui',
  standalone: true,
  templateUrl: './project-manager.ui.html',
  styleUrl: './project-manager.ui.scss',
  imports: [CommonModule, RouterLink]
})
export class ProjectManagerUI {
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
