import { Injectable, Inject, PLATFORM_ID, signal } from '@angular/core';
import { isPlatformBrowser } from '@angular/common';

@Injectable({ providedIn: 'root' })
export class AuthService {
  private isBrowser: boolean;

  private _token = signal<string | null>(null);
  private _userName = signal<string | null>(null);
  private _email = signal<string | null>(null);
  private _picture = signal<string | null>(null);
  private _role = signal<string | null>(null);
  private _status = signal<string | null>(null);

  constructor(@Inject(PLATFORM_ID) platformId: Object) {
    this.isBrowser = isPlatformBrowser(platformId);

    if (this.isBrowser) {
      const storedToken = localStorage.getItem('token');
      const storedUser = localStorage.getItem('userName');
      const storedEmail = localStorage.getItem('email');
      const storedPicture = localStorage.getItem('picture');
      const storedRole = localStorage.getItem('role');
      const storedStatus = localStorage.getItem('status');

      this._status.set(storedStatus);
      this._token.set(storedToken);
      this._userName.set(storedUser);
      this._email.set(storedEmail);
      this._picture.set(storedPicture);
      this._role.set(storedRole);
    }
  }

  setToken(token: string, userName: string | null, email: string | null, picture: string | null, role: string | null,status: string | null ) {
    this._token.set(token);
    this._userName.set(userName);
    this._email.set(email);
    this._picture.set(picture);
    this._role.set(role);
    this._status.set(status);

    if (this.isBrowser) {
      localStorage.setItem('token', token);
      if (userName) localStorage.setItem('userName', userName);
      if (email) localStorage.setItem('email', email);
      if (picture) localStorage.setItem('picture', picture);
      if (role) localStorage.setItem('role', role);
      if (status) localStorage.setItem('status', status);
    }
  }

  get token(): string | null {
    return this._token();
  }

  get isLoggedIn(): boolean {
    return !!this._token();
  }

  get userName(): string | null {
    return this._userName();
  }

  logout() {
    this._token.set(null);
    this._userName.set(null);
    this._email.set(null);
    this._picture.set(null);
    this._role.set(null);
    this._status.set(null);

    if (this.isBrowser) {
      localStorage.removeItem('token');
      localStorage.removeItem('userName');
      localStorage.removeItem('email');
      localStorage.removeItem('picture');
      localStorage.removeItem('role');
      localStorage.removeItem('status');
    }
  }

  get email(): string | null{
    return this._email();
  }

  get picture(): string | null{
    return this._picture();
  }

  get role(): string | null{
    return this._role();
  }
  get status(): string | null{
    return this._status();
  }
}
