import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
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

  constructor(private adminService: AdminService, private translation: TranslationService,private auth: AuthService) {}

  t(key: string) {
    return this.translation.translate(key);
  }

  checkUser() {
    if (!this.email.trim()) {
      alert("Email cannot be empty.");
      return;
    }

    if (this.email === this.auth.email) {
      alert("You cannot modify your own role.");
      return;
    }

    this.adminService.checkUser(this.email).subscribe({
      next: (res) => {
        this.userChecked = true;
        this.userExists = res.exists;
        this.newUser.email = this.email;
      },
      error: () => {
        alert("Server error checking user.");
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
      next: () => alert(' User created, activation email sent.'),
      error: () => alert(' Error creating user.')
    });
  }

  updateUserRole() {
    this.adminService.updateUserRole(this.email, this.updateRole).subscribe({
      next: () => alert(' Role updated, activation email sent.'),
      error: () => alert(' Error updating user role.')
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
