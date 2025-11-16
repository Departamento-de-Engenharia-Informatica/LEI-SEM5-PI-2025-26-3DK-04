import { Component, OnInit, Inject, PLATFORM_ID } from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { HttpClient } from '@angular/common/http';
import { Router, ActivatedRoute } from '@angular/router';
import { TranslationService } from '../../translation.service';

@Component({
  selector: 'app-activation-success',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './activation-success.html',
  styleUrls: ['./activation-success.scss']
})
export class ActivationSuccess implements OnInit {
  status: 'pending' | 'success' | 'error' = 'pending';
  message = '';
  currentLang = 'en';

  constructor(
    private route: ActivatedRoute,
    private http: HttpClient,
    private router: Router,
    public translation: TranslationService,
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

  ngOnInit(): void {
    this.route.queryParamMap.subscribe(params => {
      const status = params.get('status');

      if (status === 'success') {
        this.status = 'success';
        this.message = this.translation.translate('activation.successMessage');
      }
      else if (status === 'error') {
        this.status = 'error';
        this.message = this.translation.translate('activation.errorMessage');
      }
      else {
        this.status = 'error';
        this.message = this.translation.translate('activation.invalidMessage');
      }
    });
  }

  backHome() {
    this.router.navigate(['/']);
  }
}
