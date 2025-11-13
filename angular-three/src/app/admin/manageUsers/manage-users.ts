import { Component, Inject, PLATFORM_ID } from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { RouterLink } from '@angular/router';
import { TranslationService } from '../../translation.service';

@Component({
  selector: 'app-manage-users',
  standalone: true,
  imports: [CommonModule, RouterLink],
  templateUrl: './manage-users.html',
  styleUrls: ['./manage-users.scss']
})
export class ManageUsers {
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
