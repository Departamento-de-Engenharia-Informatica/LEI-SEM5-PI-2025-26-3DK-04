import { Routes } from '@angular/router';
import { Home } from './home/home';
import { Cube } from './cube/cube';
import { DockView } from './dock-view/dock-view';
import { authGuard } from './guards/auth.guard';

export const routes: Routes = [
  { path: '', component: Home },
  { path: 'cube', component: Cube },
  { 
    path: 'dock', 
    component: DockView,
    canActivate: [authGuard]  // Temporariamente desativado para desenvolvimento
  },
];
