import { Component, Inject, PLATFORM_ID } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { TranslationService } from '../../translation.service';
import { AdminService } from '../admin.service';
import { AuthService } from '../../auth.service';

@Component({
  selector: 'app-manage-users',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './manage-users.html',
  styleUrls: ['./manage-users.scss']
})
export class ManageUsers {
  email = '';
  userChecked = false;
  userExists = false;
  updateRole = '';
  newUser = { email: '', name: '', picture: '', role: 'NoRole' };
  currentLang = 'en';

  constructor(
    private adminService: AdminService,
    private translation: TranslationService,
    private auth: AuthService,
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

  checkUser() {
    if (!this.email.trim()) {
      alert(this.translation.translate('manageUsers.emailEmpty'));
      return;
    }

    if (this.email === this.auth.email) {
      alert(this.translation.translate('manageUsers.selfModifyError'));
      return;
    }

    this.adminService.checkUser(this.email).subscribe({
      next: (res) => {
        this.userChecked = true;
        this.userExists = res.exists;
        this.newUser.email = this.email;
      },
      error: () => {
        alert(this.translation.translate('manageUsers.serverError'));
      }
    });
  }

  createUser() {
    const payload = {
      email: this.email,
      name: this.newUser.name,
      picture: this.newUser.picture,
      role: this.newUser.role
    };

    this.adminService.createUser(payload).subscribe({
      next: () => alert(this.translation.translate('manageUsers.createSuccess')),
      error: () => alert(this.translation.translate('manageUsers.createError'))
    });
  }

  updateUserRole() {
    this.adminService.updateUserRole(this.email, this.updateRole).subscribe({
      next: () => alert(this.translation.translate('manageUsers.updateSuccess')),
      error: () => alert(this.translation.translate('manageUsers.updateError'))
    });
  }

  resetForm() {
    this.email = '';
    this.userChecked = false;
    this.userExists = false;
    this.newUser = { email: '', name: '', picture: '', role: 'NoRole' };
    this.updateRole = '';
  }
}
