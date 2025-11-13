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
import { ManageUsers } from './admin/manageUsers/manage-users';
import { ManageDocks } from './admin/ManageDocks/manageDocks';
import { ManageStorageAreas } from './admin/manageStorageAreas/manageStorageAreas';
import { ManageStaffMembers } from './admin/manageStaffMembers/manageStaffMembers';
import { ManageQualifications } from './admin/manageQualifications/manageQualifications';

import { ActivationSuccess } from './admin/activation-success/activation-success';
import { ManageOrganizations } from './admin/manage-organizations/manage-organizations';
import { ManageRepresentatives } from './admin/manage-representatives/manage-representatives';
import { ActivateRepresentative } from './admin/manage-representatives/activate-representative/activate-representative';
import { DeactivateRepresentative } from './admin/manage-representatives/deactivate-representative/deactivate-representative';
import { EditRepresentative } from './admin/manage-representatives/edit-representative/edit-representative';
import { AddRepresentative } from './admin/manage-representatives/add-representative/add-representative';
import { EditUser } from './admin/manageUsers/edit-user/edit-user';
import { AddUser } from './admin/manageUsers/add-user/add-user';
export const routes: Routes = [
  { path: '', component: Home },
  { path: 'cube', component: Cube },
  {
    path: 'vessel-scheduling',
    component: VesselSchedulingComponent
  },

  { path: 'dock', component: DockView },

  {
    path: 'admin',
    component: AdminUI,
    canActivate: [RoleGuard],
    data: { roles: ['admin'] }
  },
  {
    path: 'admin/manage-users',
    component: ManageUsers,
    canActivate: [RoleGuard],
    data: { roles: ['admin'] }
  },
  {
    path: 'admin/manage-users/edit-user',
    component: EditUser,
    canActivate: [RoleGuard],
    data: { roles: ['admin'] }
  },
  {
    path: 'admin/manage-users/add-user',
    component: AddUser,
    canActivate: [RoleGuard],
    data: { roles: ['admin'] }
  },

  {
    path: 'admin/manage-organizations',
    component: ManageOrganizations,
    canActivate: [RoleGuard],
    data: { roles: ['admin'] }
  },

  {
    path: 'admin/manage-representatives',
    component: ManageRepresentatives,
    canActivate: [RoleGuard],
    data: { roles: ['admin'] }
  },
  {
    path: 'admin/manage-representatives/add-representative',
    component: AddRepresentative,
    canActivate: [RoleGuard],
    data: { roles: ['admin'] }
  },
  {
    path: 'admin/manage-representatives/edit-representative',
    component: EditRepresentative,
    canActivate: [RoleGuard],
    data: { roles: ['admin'] }
  },
  {
    path: 'admin/manage-representatives/deactivate-representative',
    component: DeactivateRepresentative,
    canActivate: [RoleGuard],
    data: { roles: ['admin'] }
  },
  {
    path: 'admin/manage-representatives/activate-representative',
    component: ActivateRepresentative,
    canActivate: [RoleGuard],
    data: { roles: ['admin'] }
  },

  {
    path: 'admin/manage-docks',
    component: ManageDocks,
    canActivate: [RoleGuard],
    data: { roles: ['admin', 'projectmanager'] } // allow whoever should be able to manage docks
  },
  {
    path: 'admin/manage-storage-areas',
    component: ManageStorageAreas,
    canActivate: [RoleGuard],
    data: { roles: ['admin', 'logisticsoperator'] }
  },
  {
    path: 'admin/manage-staff-members',
    component: ManageStaffMembers,
    canActivate: [RoleGuard],
    data: { roles: ['admin', 'projectmanager'] }
  },
  {
    path: 'admin/manage-qualifications',
    component: ManageQualifications,
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
  { path: 'activate', component: ActivationSuccess },

  { path: 'access-denied', component: AccessDenied },

  { path: '**', redirectTo: '', pathMatch: 'full' }
];
