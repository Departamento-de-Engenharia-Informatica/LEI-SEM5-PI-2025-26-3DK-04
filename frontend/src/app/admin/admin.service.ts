import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable, throwError } from 'rxjs';
import { AuthService } from '../auth.service';
import { map } from 'rxjs/operators';

@Injectable({ providedIn: 'root' })
export class AdminService {
  private userBaseUrl = 'http://vs592.dei.isep.ipp.pt:5000/api/UserManagement';

  private baseUrl = 'http://vs592.dei.isep.ipp.pt:5000/api';
  // DOCK management routes
  private dockBaseUrl = 'http://vs592.dei.isep.ipp.pt:5000/api/Dock';
  private vesselTypeBaseUrl = 'http://vs592.dei.isep.ipp.pt:5000/api/VesselTypes';

  private storageAreaBaseUrl = 'http://vs592.dei.isep.ipp.pt:5000/api/StorageArea';

  private staffMembersBaseUrl = 'http://vs592.dei.isep.ipp.pt:5000/api/StaffMembers';
  private qualificationsBaseUrl = 'http://vs592.dei.isep.ipp.pt:5000/api/Qualifications';

  private organizationBaseUrl = 'http://vs592.dei.isep.ipp.pt:5000/api/Organizations';
  private representativeBaseUrl = 'http://vs592.dei.isep.ipp.pt:5000/api/Representatives';
  private vesselVisitNotificationBaseUrl = 'http://vs592.dei.isep.ipp.pt:5000/api/VesselVisitNotifications';

  private vesselBaseUrl = 'http://vs592.dei.isep.ipp.pt:5000/api/Vessels';
  private physicalResourcesBaseUrl = 'http://vs592.dei.isep.ipp.pt:5000/api/PhysicalResources';

  constructor(private http: HttpClient, private auth: AuthService) {}

  /* ===============================
            USER MANAGEMENT
  =============================== */

  checkUser(email: string): Observable<any> {
    const loggedEmail = this.auth.email;
    if (loggedEmail && email === loggedEmail) {
      return throwError(() => ({ error: 'You cannot modify your own role.' }));
    }
    return this.http.get(`${this.userBaseUrl}/check/${email}`);
  }

  createUser(user: any): Observable<any> {
    return this.http.post(`${this.userBaseUrl}/create`, user);
  }

  getAllUsers(): Observable<any[]> {
    return this.http.get<any[]>(`${this.userBaseUrl}/get`);
  }

  updateUserRole(email: string, role: string): Observable<any> {
    const loggedEmail = this.auth.email;
    if (loggedEmail && email === loggedEmail) {
      return throwError(() => ({ error: 'You cannot update your own role.' }));
    }
    return this.http.put(`${this.userBaseUrl}/${email}/role`, { role });
  }

  GetUserByEmail( email :string):Observable<any[]>{
    return this.http.get<any[]>(`${this.userBaseUrl}/get/${email}`);
  }

  /* ===============================
            DOCK MANAGEMENT
  =============================== */

  getAllDocks(): Observable<any[]> {
    return this.http.get<any[]>(`${this.dockBaseUrl}`);
  }

  getDockById(id: string): Observable<any> {
    return this.http.get(`${this.dockBaseUrl}/${id}`);
  }

  createDock(dto: any): Observable<any> {
    return this.http.post(`${this.dockBaseUrl}`, dto);
  }

  updateDock(id: string, dto: any): Observable<any> {
    return this.http.put(`${this.dockBaseUrl}/${id}`, dto);
  }

  softDeleteDock(id: string): Observable<any> {
    return this.http.delete(`${this.dockBaseUrl}/${id}`);
  }

  hardDeleteDock(id: string): Observable<any> {
    return this.http.delete(`${this.dockBaseUrl}/${id}/hard`);
  }

  /* ===============================
            VESSEL TYPES
  =============================== */
  getVesselTypes(): Observable<any[]> {
    return this.http.get<any[]>(`${this.vesselTypeBaseUrl}`);
  }

  /* ===============================
            STORAGE AREAS
  =============================== */
  getAllStorageAreas(): Observable<any[]> {
    return this.http.get<any[]>(`${this.storageAreaBaseUrl}`);
  }

  createStorageArea(dto: any): Observable<any> {
    return this.http.post(`${this.storageAreaBaseUrl}`, dto);
  }

  updateStorageArea(id: string, dto: any): Observable<any> {
    return this.http.put(`${this.storageAreaBaseUrl}${id}`, dto);
  }

  inactivateStorageArea(id: string): Observable<any> {
    return this.http.patch(`${this.storageAreaBaseUrl}${id}/inactivate`, {});
  }

  getAllYards(): Observable<any[]> {
    return this.http.get<any[]>(`${this.storageAreaBaseUrl}/yard`);
  }

  // NOVO MÉTODO: Obter apenas Armazéns (Warehouses)
  getAllWarehouses(): Observable<any[]> {
    return this.http.get<any[]>(`${this.storageAreaBaseUrl}/warehouse`);
  }
  /* ===============================
        STAFF MEMBERS MANAGEMENT
  =============================== */

  getAllStaffMembers(): Observable<any[]> {
    return this.http.get<any[]>(`${this.staffMembersBaseUrl}`);
  }

  getStaffMemberById(id: string): Observable<any> {
    return this.http.get(`${this.staffMembersBaseUrl}/${id}`);
  }

  createStaffMember(dto: any): Observable<any> {
    return this.http.post(`${this.staffMembersBaseUrl}`, dto);
  }

  updateStaffMember(id: string, dto: any): Observable<any> {
    return this.http.put(`${this.staffMembersBaseUrl}/${id}`, dto);
  }

  deactivateStaffMember(id: string): Observable<any> {
    return this.http.delete(`${this.staffMembersBaseUrl}/${id}`);
  }

  reactivateStaffMember(id: string): Observable<any> {
    return this.http.put(`${this.staffMembersBaseUrl}/${id}/reactivate`, {});
  }

  addQualificationToStaff(staffId: string, qualificationId: string): Observable<any> {
    return this.http.post(`${this.staffMembersBaseUrl}/${staffId}/qualifications`, {
      QualificationId: qualificationId
    });
  }

  removeQualificationFromStaff(staffId: string, qualificationId: string): Observable<any> {
    return this.http.delete(`${this.staffMembersBaseUrl}/${staffId}/qualifications/${qualificationId}`);
  }

  searchStaffMembers(name?: string, status?: string, qualificationId?: string): Observable<any[]> {
    let params = '';
    if (name) params += `name=${name}&`;
    if (status) params += `status=${status}&`;
    if (qualificationId) params += `qualificationId=${qualificationId}&`;

    return this.http.get<any[]>(`${this.staffMembersBaseUrl}/search?${params}`);
  }

  /* ===============================
        QUALIFICATIONS MANAGEMENT
  =============================== */

  getAllQualifications(): Observable<any[]> {
    return this.http.get<any[]>(`${this.qualificationsBaseUrl}`);
  }

  getQualificationById(id: string): Observable<any> {
    return this.http.get(`${this.qualificationsBaseUrl}/${id}`);
  }

  createQualification(dto: any): Observable<any> {
    return this.http.post(`${this.qualificationsBaseUrl}`, dto);
  }

  updateQualification(id: string, dto: any): Observable<any> {
    return this.http.put(`${this.qualificationsBaseUrl}/${id}`, dto);
  }

  deleteQualification(id: string): Observable<any> {
    return this.http.delete(`${this.qualificationsBaseUrl}/${id}`);
  }

  /* ===============================
        ORGANIZATION MANAGEMENT
  =============================== */

  getAllOrganizations(): Observable<any[]> {
    return this.http.get<any[]>(`${this.organizationBaseUrl}`);
  }

  getOrganizationById(id: string): Observable<any> {
    return this.http.get(`${this.organizationBaseUrl}/${id}`);
  }

  createOrganization(dto: any): Observable<any> {
    return this.http.post(`${this.organizationBaseUrl}`, dto);
  }

  checkOrganizationLegalNameExists(name: string): Observable<boolean> {
    return this.http.get<boolean>(`${this.organizationBaseUrl}/check-legalname/${encodeURIComponent(name)}`).pipe(
      map(res => !!res)
    );
  }

  checkOrganizationTaxNumberExists(taxNumber: string): Observable<boolean> {
    return this.http.get<boolean>(`${this.organizationBaseUrl}/check-taxnumber?taxNumber=${encodeURIComponent(taxNumber)}`).pipe(
      map(res => !!res)
    );
  }

  /* ===============================
        REPRESENTATIVE MANAGEMENT
  =============================== */

  getAllRepresentatives(): Observable<any[]> {
    return this.http.get<any[]>(`${this.representativeBaseUrl}`);
  }

  getRepresentativeById(id: string): Observable<any> {
    return this.http.get(`${this.representativeBaseUrl}/${id}`);
  }

  createRepresentative(dto: any): Observable<any> {
    return this.http.post(`${this.representativeBaseUrl}`, dto);
  }

  updateRepresentative(id: string, dto: any): Observable<any> {
    return this.http.put(`${this.representativeBaseUrl}/${id}/update`, dto);
  }

  activateRepresentative(id: string): Observable<any> {
    return this.http.put(`${this.representativeBaseUrl}/${id}/activate`, {});
  }

  deactivateRepresentative(id: string): Observable<any> {
    return this.http.put(`${this.representativeBaseUrl}/${id}/deactivate`, {});
  }

  getActiveRepresentatives(): Observable<any[]> {
    return this.http.get<any[]>(`${this.representativeBaseUrl}/active`);
  }

  getInactiveRepresentatives(): Observable<any[]> {
    return this.http.get<any[]>(`${this.representativeBaseUrl}/inactive`);
  }

  checkRepresentativeEmailExists(email: string): Observable<boolean> {
    return this.http.get<boolean>(`${this.representativeBaseUrl}/check-email/${encodeURIComponent(email)}`).pipe(
      map(res => !!res)
    );
  }

  checkRepresentativeCitizenIdExists(cid: string): Observable<boolean> {
    return this.http.get<boolean>(`${this.representativeBaseUrl}/check-citizenid/${encodeURIComponent(cid)}`).pipe(
      map(res => !!res)
    );
  }

  checkRepresentativePhoneExists(phone: string): Observable<boolean> {
    return this.http.get<boolean>(`${this.representativeBaseUrl}/check-phone/${encodeURIComponent(phone)}`).pipe(
      map(res => !!res)
    );
  }
  /* ===============================
    PHYSICAL RESOURCES MANAGEMENT
=============================== */

  getAllPhysicalResources(): Observable<any[]> {
    return this.http.get<any[]>(`${this.physicalResourcesBaseUrl}`);
  }

  getPhysicalResourceById(id: string): Observable<any> {
    return this.http.get(`${this.physicalResourcesBaseUrl}/${id}`);
  }

  createPhysicalResource(dto: any): Observable<any> {
    return this.http.post(`${this.physicalResourcesBaseUrl}`, dto);
  }

  updatePhysicalResource(id: string, dto: any): Observable<any> {
    return this.http.put(`${this.physicalResourcesBaseUrl}/${id}`, dto);
  }

  updatePhysicalResourceStatus(id: string, status: string): Observable<any> {
    return this.http.patch(
      `${this.physicalResourcesBaseUrl}/${id}/status`,
      { status }
    );
  }
  /* ===============================
    VESSEL VISIT NOTIFICATION MANAGEMENT
  =============================== */

  createVesselVisitNotification(dto: any): Observable<any> {
    return this.http.post(`${this.vesselVisitNotificationBaseUrl}`, dto);
  }
  getAllVesselVisitNotifications(): Observable<any[]> {
    return this.http.get<any[]>(`${this.vesselVisitNotificationBaseUrl}`);
  }
  getVesselVisitNotificationById(id: string): Observable<any> {
    return this.http.get(`${this.vesselVisitNotificationBaseUrl}/${id}`);
  }
  submitVesselVisitNotification(id: string): Observable<any> {
    return this.http.put(`${this.vesselVisitNotificationBaseUrl}/${id}/submit`, {});
  }
  getSubmittedVesselVisitNotifications() {
    return this.http.get<any[]>(`${this.baseUrl}/VesselVisitNotifications/submitted`);
  }


  // APROVAR notificação
  approveVesselVisitNotification(id: string, dockId: string, officerId: string): Observable<any> {
    return this.http.put(`${this.vesselVisitNotificationBaseUrl}/${id}/approve`, {
      DockId: dockId,
      OfficerId: officerId
    });
  }

  // REJEITAR notificação
  rejectVesselVisitNotification(id: string, reason: string, officerId: string): Observable<any> {
    return this.http.put(`${this.vesselVisitNotificationBaseUrl}/${id}/reject`, {
      Reason: reason,
      OfficerId: officerId
    });
  }

  getVessels(): Observable<any[]> { return this.http.get<any[]>(this.vesselBaseUrl); }

  getInProgressVesselVisitNotifications() {
    return this.http.get<any[]>(`${this.baseUrl}/VesselVisitNotifications/in-progress`);
  }

  getWithdrawnVesselVisitNotifications() {
    return this.http.get<any[]>(`${this.vesselVisitNotificationBaseUrl}/withdrawn`);
  }

  // FUNÇÃO NECESSÁRIA PARA O REQUISITO 1 (EDIÇÃO)
  updateVesselVisitNotificationInProgress(id: string, dto: any): Observable<any> {
    return this.http.put(`${this.vesselVisitNotificationBaseUrl}/${id}/update`, dto);
  }

  // FUNÇÃO NECESSÁRIA PARA O REQUISITO 1 (WITHDRAW)
  withdrawVesselVisitNotification(id: string): Observable<any> {
    return this.http.put(`${this.vesselVisitNotificationBaseUrl}/${id}/withdraw`, {});
  }

  // FUNÇÃO NECESSÁRIA PARA O REQUISITO 2 (RESUME)
  resumeVesselVisitNotification(id: string): Observable<any> {
    return this.http.put(`${this.vesselVisitNotificationBaseUrl}/${id}/resume`, {});
  }

}
