import { Component, Inject, PLATFORM_ID } from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { RouterLink } from '@angular/router';
import { TranslationService } from '../translation.service';

@Component({
  selector: 'app-admin-ui',
  standalone: true,
  imports: [CommonModule, RouterLink],
  templateUrl: './admin.ui.html',
  styleUrls: ['./admin.ui.scss']
})
export class AdminUI {
  currentLang = 'en';

  sections = [
    { name: 'User Management' },
    { name: 'System Logs' },
    { name: 'Settings' },
    {name: 'Dock Management'}
  ];

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
