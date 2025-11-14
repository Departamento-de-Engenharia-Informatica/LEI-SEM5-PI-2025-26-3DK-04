import { Component, Inject, PLATFORM_ID } from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { AdminService } from '../../admin.service';
import { TranslationService } from '../../../translation.service';
import { finalize } from 'rxjs/operators';

@Component({
  selector: 'app-add-user',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './add-user.html',
  styleUrls: ['./add-user.scss']
})
export class AddUser {
  newUser: any = { email: '', name: '', role: 'NoRole' };
  messageSuccess = '';
  messageError = '';
  currentLang = 'en';
  errors: any = {};
  loading = false;

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

  validateEmail() {
    this.errors.email = '';
    const email = (this.newUser.email || '').trim();
    if (!email) { this.errors.email = this.t('manageUsers.emailRequired'); return false; }
    // ensure ends with @gmail.com
    if (!/^[^\s@]+@gmail\.com$/i.test(email)) {
      this.errors.email = this.t('manageUsers.emailMustBeGmail') || 'Email must end with @gmail.com';
      return false;
    }
    return true;
  }

  validateName() {
    this.errors.name = '';
    if (!this.newUser.name || !this.newUser.name.trim()) {
      this.errors.name = this.t('manageUsers.nameRequired');
      return false;
    }
    return true;
  }

  createUser() {
    this.messageError = '';
    this.messageSuccess = '';
    this.errors = {};

    const okEmail = this.validateEmail();
    const okName = this.validateName();
    if (!okEmail || !okName) return;

    this.loading = true;

    // Optional: check if email already exists (if adminService exposes it)
    const payload = { ...this.newUser };

    this.adminService.createUser(payload).pipe(
      finalize(() => this.loading = false)
    ).subscribe({
      next: () => {
        // show message that activation email was sent
        this.messageSuccess = this.t('manageUsers.createSuccessActivation') || 'User created; activation email was sent successfully.';
        // clear fields
        this.newUser = { email: '', name: '', role: 'NoRole' };
        this.errors = {};
      },
      error: err => {
        console.error(err);
        this.messageError = this.t('manageUsers.createError') || 'Error creating user.';
      }
    });
  }

  goBack() {
    this.router.navigate(['/admin/manage-users']);
  }
}
