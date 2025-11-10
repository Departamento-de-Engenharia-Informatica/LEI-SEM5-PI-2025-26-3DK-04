import { Routes } from '@angular/router';
import { Home } from './home/home';
import { Cube } from './cube/cube';
import { DockView } from './dock-view/dock-view';
import { RoleGuard } from './guards/role.guard';
import { AdminUI } from './admin/admin.ui';
import { AccessDenied } from './acess-denied/access-denied';
import { VesselSchedulingComponent } from './vessel-scheduling/vessel-scheduling.component';

export const routes: Routes = [
  { path: '', component: Home },
  { path: 'cube', component: Cube },
  { path: 'vessel-scheduling', component: VesselSchedulingComponent },
  { path: 'dock', component: DockView },

  {
    path: 'admin',
    component: AdminUI,
    canActivate: [RoleGuard],
    data: { roles: ['admin'] },
    canMatch: [() => { console.log('[Route] Matching admin route'); return true; }]
  },

  { path: 'access-denied', component: AccessDenied }
];
