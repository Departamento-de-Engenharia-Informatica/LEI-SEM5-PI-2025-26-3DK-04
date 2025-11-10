import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterLink } from '@angular/router';
import { TranslationService } from '../translation.service';
import { AuthService } from "../auth.service";
@Component({
  selector: 'app-home',
  standalone: true,
  imports: [CommonModule, RouterLink],
  templateUrl: './home.html',
  styleUrl: './home.scss',
})
export class Home {
  constructor(private translation: TranslationService,private authService : AuthService) {}

  t(key: string): any {
    return this.translation.translate(key);
  }
  get userName(): string | null {
    return this.authService.userName;
  }
  get email(): string | null {
    return this.authService.email;
  }
  get role(): string | null {
    return this.authService.role;
  }
  get picture(): string | null {
    return this.authService.picture;
  }
  get isLoggedIn(): boolean {
    return this.authService.isLoggedIn;
  }
  get status(): string | null {
    return this.authService.status;
  }
}
