import { Injectable, Inject, PLATFORM_ID } from '@angular/core';
import { isPlatformBrowser } from '@angular/common';
import { OAuthService, AuthConfig } from 'angular-oauth2-oidc';
import { Router } from '@angular/router';

@Injectable({
  providedIn: 'root'
})
export class AuthService {

  private isBrowser: boolean;

  constructor(
    private oauthService: OAuthService,
    private router: Router,
    @Inject(PLATFORM_ID) private platformId: Object
  ) {
    this.isBrowser = isPlatformBrowser(this.platformId);
  }

  initOAuth(): void {
    if (!this.isBrowser) return;

    console.log("⚙️ [Auth] Initializing OAuth setup...");

    const authConfig: AuthConfig = {
      issuer: 'https://accounts.google.com',
      redirectUri: window.location.origin + '/',
      clientId: '440853175141-2kp1hrvoe78b8pn597p8oc2b316dibq5.apps.googleusercontent.com',
      responseType: 'code',
      disablePKCE: false,
      scope: 'openid profile email',
      showDebugInformation: true,
      strictDiscoveryDocumentValidation: false,
    };

    this.oauthService.configure(authConfig as AuthConfig);

    this.oauthService.loadDiscoveryDocumentAndTryLogin().then(() => {
      console.log("🔐 Logged in:", this.isLoggedIn);
      console.log("🔑 access token:", this.oauthService.getAccessToken());
      console.log("🪪 identity claims:", this.oauthService.getIdentityClaims());
    });
  }

  login(): void {
    console.log("🔵 [Auth] Login() called — redirecting to auth provider...");
    if (this.isBrowser) this.oauthService.initCodeFlow();
  }

  logout(): void {
    console.log("🟠 [Auth] Logout() called — clearing tokens...");
    if (this.isBrowser) this.oauthService.logOut();
  }

  get token(): string | null {
    return this.isBrowser ? this.oauthService.getAccessToken() : null;
  }

  get isLoggedIn(): boolean {
    const logged = !!this.oauthService.getAccessToken();
    console.log("👀 [Auth] Checking login state:", logged);
    return logged;
  }

  getClaims(): any {
    if (!this.isBrowser) return null;
    return this.oauthService.getIdentityClaims() as any;
  }
}
