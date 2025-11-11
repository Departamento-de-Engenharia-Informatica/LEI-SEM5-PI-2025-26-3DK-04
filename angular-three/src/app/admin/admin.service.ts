import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable, throwError } from 'rxjs';
import { AuthService } from '../auth.service';

@Injectable({ providedIn: 'root' })
export class AdminService {
  private baseUrl = 'https://localhost:5001/api/UserManagement';

  constructor(private http: HttpClient, private auth: AuthService) {}

  checkUser(email: string): Observable<any> {
    console.log(`[checkUser] Checking: ${email}`);

    const loggedEmail = this.auth.email;

    if (loggedEmail && email === loggedEmail) {
      return throwError(() => ({ error: 'You cannot modify your own role.' }));
    }

    return this.http.get(`${this.baseUrl}/check/${email}`);
  }

  createUser(user: any): Observable<any> {
    console.log('[createUser] Sending:', user);
    return this.http.post(`${this.baseUrl}/create`, user);
  }

  updateUserRole(email: string, role: string): Observable<any> {
    console.log(`[updateUserRole] Updating ${email} to role ${role}`);

    const loggedEmail = this.auth.email;

    if (loggedEmail && email === loggedEmail) {
      return throwError(() => ({ error: 'You cannot update your own role.' }));
    }

    return this.http.put(`${this.baseUrl}/${email}/role`, { role });
  }
}
