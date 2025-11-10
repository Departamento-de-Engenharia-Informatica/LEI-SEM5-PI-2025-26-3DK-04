import { Routes } from '@angular/router';
import { Home } from './home/home';
import { Cube } from './cube/cube';
import { DockView } from './dock-view/dock-view';
import { RoleGuard } from './guards/role.guard';
import { AdminUI } from './admin/admin.ui';
import { AccessDenied } from './acess-denied/access-denied';
import { VesselSchedulingComponent } from './vessel-scheduling/vessel-scheduling.component';
import { RepresentativeUI } from './Representative/representative.ui';
import { PortAuthorityUI } from './PortAuthorityOfficer/port-authority.ui';
import { LogisticsUI } from './Logistics-Operator/logistics.ui';
import { ProjectManagerUI } from './ProjectManager/project-manager.ui';

export const routes: Routes = [
  { path: '', component: Home },
  { path: 'cube', component: Cube },
  { path: 'vessel-scheduling', component: VesselSchedulingComponent },
  { path: 'dock', component: DockView },

  {
    path: 'admin',
    component: AdminUI,
    canActivate: [RoleGuard],
    data: { roles: ['admin'] }
  },

  {
    path: 'representative',
    component: RepresentativeUI,
    canActivate: [RoleGuard],
    data: { roles: ['representative','admin'] }
  },

  {
    path: 'port-officer',
    component: PortAuthorityUI,
    canActivate: [RoleGuard],
    data: { roles: ['portauthorityofficer','admin'] }
  },

  {
    path: 'logistics',
    component: LogisticsUI,
    canActivate: [RoleGuard],
    data: { roles: ['logisticsoperator','admin'] }
  },

  {
    path: 'project-manager',
    component: ProjectManagerUI,
    canActivate: [RoleGuard],
    data: { roles: ['projectmanager','admin'] }
  },

  { path: 'access-denied', component: AccessDenied }
];
