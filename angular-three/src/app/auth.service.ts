import { Injectable, Inject, PLATFORM_ID, signal } from '@angular/core';
import { isPlatformBrowser } from '@angular/common';

@Injectable({ providedIn: 'root' })
export class AuthService {
  private isBrowser: boolean;

  private _token = signal<string | null>(null);
  private _userName = signal<string | null>(null);

  constructor(@Inject(PLATFORM_ID) platformId: Object) {
    this.isBrowser = isPlatformBrowser(platformId);

    if (this.isBrowser) {
      const storedToken = localStorage.getItem('token');
      const storedUser = localStorage.getItem('userName');

      this._token.set(storedToken);
      this._userName.set(storedUser);
    }
  }

  setToken(token: string, userName: string | null) {
    this._token.set(token);
    this._userName.set(userName);

    if (this.isBrowser) {
      localStorage.setItem('token', token);
      if (userName) localStorage.setItem('userName', userName);
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

    if (this.isBrowser) {
      localStorage.removeItem('token');
      localStorage.removeItem('userName');
    }
  }
}
