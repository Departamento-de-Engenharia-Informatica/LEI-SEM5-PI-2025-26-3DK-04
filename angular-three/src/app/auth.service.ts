import {Injectable, signal} from '@angular/core';

@Injectable({ providedIn: 'root' })
export class AuthService {
  private _token = signal<string | null>(null);
  private _userName = signal<string | null>(null);

  setToken(token: string, userName: string | null) {
    this._token.set(token);
    this._userName.set(userName);
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
  }
}
