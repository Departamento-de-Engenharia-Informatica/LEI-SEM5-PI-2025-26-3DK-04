import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

export interface Vessel {
  id: string;
  imoNumber: string;
  name: string;
  vesselTypeId: string;
  vesselTypeName: string;
  owner: string;
  operator: string;
  active: boolean;
}

export interface CreateVesselDto {
  imoNumber: string;
  name: string;
  vesselTypeId: string;
  owner: string;
  operator: string;
}

export interface UpdateVesselDto {
  imoNumber?: string;
  name?: string;
  vesselTypeId?: string;
  owner?: string;
  operator?: string;
}

@Injectable({ providedIn: 'root' })
export class PortAuthorityOfficerService {
  private vesselsBaseUrl = 'https://localhost:5001/api/Vessels';
  private baseUrl = 'https://localhost:5001/api';

  constructor(private http: HttpClient) {}

  /* ===============================
            VESSEL MANAGEMENT
  =============================== */

  getAllVessels(search?: string): Observable<Vessel[]> {
    const url = search ? `${this.vesselsBaseUrl}?search=${encodeURIComponent(search)}` : this.vesselsBaseUrl;
    return this.http.get<Vessel[]>(url);
  }

  getVesselById(id: string): Observable<Vessel> {
    return this.http.get<Vessel>(`${this.vesselsBaseUrl}/${id}`);
  }

  getVesselByImoNumber(imoNumber: string): Observable<Vessel> {
    return this.http.get<Vessel>(`${this.vesselsBaseUrl}/imo/${imoNumber}`);
  }

  searchVesselsByName(term: string): Observable<Vessel[]> {
    return this.http.get<Vessel[]>(`${this.vesselsBaseUrl}/search/name?term=${encodeURIComponent(term)}`);
  }

  searchVesselsByOwner(term: string): Observable<Vessel[]> {
    return this.http.get<Vessel[]>(`${this.vesselsBaseUrl}/search/owner?term=${encodeURIComponent(term)}`);
  }

  searchVesselsByOperator(term: string): Observable<Vessel[]> {
    return this.http.get<Vessel[]>(`${this.vesselsBaseUrl}/search/operator?term=${encodeURIComponent(term)}`);
  }

  createVessel(dto: CreateVesselDto): Observable<Vessel> {
    return this.http.post<Vessel>(this.vesselsBaseUrl, dto);
  }

  updateVessel(id: string, dto: UpdateVesselDto): Observable<Vessel> {
    return this.http.put<Vessel>(`${this.vesselsBaseUrl}/${id}`, dto);
  }

  inactivateVessel(id: string): Observable<Vessel> {
    return this.http.delete<Vessel>(`${this.vesselsBaseUrl}/${id}`);
  }

  activateVessel(id: string): Observable<Vessel> {
    return this.http.post<Vessel>(`${this.vesselsBaseUrl}/${id}/activate`, {});
  }
}
