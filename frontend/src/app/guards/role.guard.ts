import { Injectable, Inject, PLATFORM_ID } from '@angular/core';
import { CanActivate, Router, ActivatedRouteSnapshot } from '@angular/router';
import { AuthService } from '../auth.service';
import { isPlatformBrowser } from '@angular/common';

@Injectable({ providedIn: 'root' })
export class RoleGuard implements CanActivate {

  constructor(
    private authService: AuthService,
    private router: Router,
    @Inject(PLATFORM_ID) private platformId: Object
  ) {}

  canActivate(route: ActivatedRouteSnapshot): boolean {

    console.log('----- ROLE GUARD TRIGGERED -----');

    if (!isPlatformBrowser(this.platformId)) {
      console.warn('RoleGuard: SSR detected → automatically allowing route');
      return true;
    }

    const expectedRaw = route.data?.['roles'];
    const expectedRoles: string[] = Array.isArray(expectedRaw)
      ? expectedRaw.map(r => String(r).trim().toLowerCase())
      : expectedRaw ? [String(expectedRaw).trim().toLowerCase()] : [];

    let userRole = this.authService.role;

    console.log('[RoleGuard] Expected roles:', expectedRoles);
    console.log('[RoleGuard] Role from service:', userRole);

    if (!userRole) {
      const storedRole = localStorage.getItem('role');
      console.log('[RoleGuard] storedRole after fallback:', storedRole);
      userRole = storedRole;
    }

    if (!userRole) {
      console.warn('[RoleGuard] No role → Access denied');
      this.router.navigate(['/access-denied']);
      return false;
    }

    const normalized = userRole.trim().toLowerCase();
    const allowed = expectedRoles.includes(normalized);

    console.log('[RoleGuard] Normalized user role:', normalized);
    console.log('[RoleGuard] Allowed?', allowed);

    if (!allowed) {
      console.warn('[RoleGuard] Role invalid → Access denied');
      this.router.navigate(['/access-denied']);
      return false;
    }

    console.log('[RoleGuard] ACCESS GRANTED');
    return true;
  }
}
