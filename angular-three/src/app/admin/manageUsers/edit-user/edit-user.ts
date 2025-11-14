import { Component, Inject, OnInit, PLATFORM_ID } from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { AdminService } from '../../admin.service';
import { TranslationService } from '../../../translation.service';
import { AuthService } from '../../../auth.service';

@Component({
  selector: 'app-edit-user',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './edit-user.html',
  styleUrls: ['./edit-user.scss']
})
export class EditUser implements OnInit {
  users: any[] = [];
  filteredUsers: any[] = [];
  searchTerm = '';
  userForm: any = null;
  messageSuccess = '';
  messageError = '';
  currentLang = 'en';

  constructor(
    private adminService: AdminService,
    private translation: TranslationService,
    private router: Router,
    private auth: AuthService,
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

  ngOnInit() {
    this.loadUsers();
  }

  loadUsers() {
    this.adminService.getAllUsers().subscribe({
      next: users => {
        const loggedEmail = this.auth.email;

        this.users = (users || []).filter(u =>
          u.role !== 'NoRole' &&
          u.role !== 'Unknown' &&
          u.email !== loggedEmail &&
          u.status !== 'Inactive' &&
          u.role !== 'Admin' &&
          u.role !== 'Representative'
        );

        this.filteredUsers = [...this.users];
      },
      error: err => {
        console.error('Load users error', err);
        this.messageError = this.t('manageUsers.loadError');
      }
    });
  }

  applyFilter() {
    const q = (this.searchTerm || '').toLowerCase();
    this.filteredUsers = this.users.filter(u =>
      (u.name || '').toLowerCase().includes(q) ||
      (u.email || '').toLowerCase().includes(q)
    );
  }

  selectUser(user: any) {
    // fill userForm with the selected user for editing
    this.userForm = { ...user };
    this.messageSuccess = '';
    this.messageError = '';
  }

  saveChanges() {
    if (!this.userForm) return;
    // call updateUserRole or generic update path if exists
    this.adminService.updateUserRole(this.userForm.email, this.userForm.role).subscribe({
      next: () => {
        // show message that activation email was sent (as requested)
        this.messageSuccess = this.t('manageUsers.updateSuccessActivation') || 'User updated; activation email was sent successfully.';
        // clear form fields
        this.userForm = null;
        this.loadUsers();
      },
      error: err => {
        console.error(err);
        this.messageError = this.t('manageUsers.updateError') || 'Error updating user.';
      }
    });
  }

  cancelEdit() {
    this.userForm = null;
    this.messageError = '';
    this.messageSuccess = '';
  }

  goBack() {
    this.router.navigate(['/admin/manage-users']);
  }
}
