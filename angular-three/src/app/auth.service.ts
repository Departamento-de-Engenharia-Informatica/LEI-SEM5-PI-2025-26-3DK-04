import { Injectable } from '@angular/core';

@Injectable({ providedIn: 'root' })
export class AuthService {
  private _token: string | null = null;
  private _userName: string | null = null;

  constructor() {}

  setToken(token: string, userName: string | null) {
    this._token = token;
    this._userName = userName;
  }

  get token(): string | null {
    return this._token;
  }

  get isLoggedIn(): boolean {
    return !!this._token;
  }

  get userName(): string | null {
    return this._userName;
  }

  logout() {
    this._token = null;
    this._userName = null;
  }
}
