import { Component, Inject, PLATFORM_ID } from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TranslationService } from '../../translation.service';
import { RouterLink } from '@angular/router';

@Component({
  selector: 'app-manage-representatives',
  standalone: true,
  imports: [CommonModule, FormsModule, RouterLink],
  templateUrl: './manage-representatives.html',
  styleUrls: ['./manage-representatives.scss']
})
export class ManageRepresentatives {

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
