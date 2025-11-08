import { inject } from '@angular/core';
import { Router, CanActivateFn } from '@angular/router';
import { AuthService } from '../auth.service';

export const authGuard: CanActivateFn = (route, state) => {
  const router = inject(Router);
  const authService = inject(AuthService);
  
  console.log('Auth Guard - isLoggedIn:', authService.isLoggedIn);
  console.log('Auth Guard - token:', authService.token);
  console.log('Auth Guard - userName:', authService.userName);
  
  if (authService.isLoggedIn) {
    return true;
  }
  
  // Redireciona para home se não estiver logado
  alert('You need to be logged in to access this page!');
  router.navigate(['/']);
  return false;
};
