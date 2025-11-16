import { Component, Inject, PLATFORM_ID } from '@angular/core';
import { TranslationService } from '../translation.service';
import { isPlatformBrowser } from '@angular/common';

@Component({
  selector: 'app-logistics-ui',
  standalone: true,
  templateUrl: './logistics.ui.html',
  styleUrl: './logistics.ui.scss'
})
export class LogisticsUI {
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
