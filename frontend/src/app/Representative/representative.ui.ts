import { Component, Inject, PLATFORM_ID } from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { TranslationService } from '../translation.service';
import {RouterLink} from '@angular/router';

@Component({
  standalone: true,
  selector: 'app-representative',
  templateUrl: './representative.ui.html',
  styleUrls: ['./representative.ui.scss'],
  imports: [CommonModule, RouterLink]
})
export class RepresentativeUI {
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
