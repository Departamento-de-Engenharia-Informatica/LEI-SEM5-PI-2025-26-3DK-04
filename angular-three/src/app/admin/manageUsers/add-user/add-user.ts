import { Component, Inject, PLATFORM_ID } from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { AdminService } from '../../admin.service';
import { TranslationService } from '../../../translation.service';

@Component({
  selector: 'app-add-user',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './add-user.html',
  styleUrls: ['./add-user.scss']
})
export class AddUser {
  newUser: any = { email: '', name: '', role: 'NoRole' };
  message = '';
  currentLang = 'en';

  constructor(
    private adminService: AdminService,
    private translation: TranslationService,
    private router: Router,
    @Inject(PLATFORM_ID) private platformId: Object
  ) {
    const isBrowser = isPlatformBrowser(this.platformId);
    if (isBrowser) {
      const savedLang = localStorage.getItem('appLang');
      if (savedLang) this.translation.setLanguage(savedLang);
      this.currentLang = savedLang || 'en';
    }
  }

  t(key: string) {
    return this.translation.translate(key);
  }

  createUser() {
    this.adminService.createUser(this.newUser).subscribe({
      next: () => {
        this.message = this.t('manageUsers.createSuccess');
        this.newUser = { email: '', name: '', role: 'NoRole' };
      },
      error: err => {
        console.error(err);
        this.message = this.t('manageUsers.createError');
      }
    });
  }

  goBack() {
    this.router.navigate(['/admin/manage-users']);
  }
}
