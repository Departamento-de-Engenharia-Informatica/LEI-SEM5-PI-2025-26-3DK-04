import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable, throwError } from 'rxjs';
import { AuthService } from '../auth.service';

@Injectable({ providedIn: 'root' })
export class AdminService {

  // USER management routes
  private userBaseUrl = 'https://localhost:5001/api/UserManagement';

  // DOCK management routes
  private dockBaseUrl = 'https://localhost:5001/api/Dock';
  private vesselTypeBaseUrl = 'https://localhost:5001/api/VesselTypes';

  private storageAreaBaseUrl = 'https://localhost:5001/api/StorageArea';
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

  updateUserRole(email: string, role: string): Observable<any> {
    const loggedEmail = this.auth.email;
    if (loggedEmail && email === loggedEmail) {
      return throwError(() => ({ error: 'You cannot update your own role.' }));
    }
    return this.http.put(`${this.userBaseUrl}/${email}/role`, { role });
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

  // STORAGE AREA management
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

}
