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

export interface VesselType {
  id: string;
  name: string;
  description: string;
  capacity: number;
  maxRows: number;
  maxBays: number;
  maxTiers: number;
  active: boolean;
}

export interface CreateVesselTypeDto {
  name: string;
  description: string;
  capacity: number;
  maxRows: number;
  maxBays: number;
  maxTiers: number;
}

export interface UpdateVesselTypeDto {
  name?: string;
  description?: string;
  capacity?: number;
  maxRows?: number;
  maxBays?: number;
  maxTiers?: number;
}

@Injectable({ providedIn: 'root' })
export class PortAuthorityOfficerService {
  private vesselsBaseUrl = 'https://localhost:5001/api/Vessels';
  private vesselTypesBaseUrl = 'https://localhost:5001/api/VesselTypes';
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

  /* ===============================
         VESSEL TYPE MANAGEMENT
  =============================== */

  getAllVesselTypes(search?: string): Observable<VesselType[]> {
    const url = search ? `${this.vesselTypesBaseUrl}?search=${encodeURIComponent(search)}` : this.vesselTypesBaseUrl;
    return this.http.get<VesselType[]>(url);
  }

  getVesselTypeById(id: string): Observable<VesselType> {
    return this.http.get<VesselType>(`${this.vesselTypesBaseUrl}/${id}`);
  }

  searchVesselTypesByName(term: string): Observable<VesselType[]> {
    return this.http.get<VesselType[]>(`${this.vesselTypesBaseUrl}/search/name?term=${encodeURIComponent(term)}`);
  }

  searchVesselTypesByDescription(term: string): Observable<VesselType[]> {
    return this.http.get<VesselType[]>(`${this.vesselTypesBaseUrl}/search/description?term=${encodeURIComponent(term)}`);
  }

  createVesselType(dto: CreateVesselTypeDto): Observable<VesselType> {
    return this.http.post<VesselType>(this.vesselTypesBaseUrl, dto);
  }

  updateVesselType(id: string, dto: UpdateVesselTypeDto): Observable<VesselType> {
    return this.http.put<VesselType>(`${this.vesselTypesBaseUrl}/${id}`, dto);
  }

  inactivateVesselType(id: string): Observable<VesselType> {
    return this.http.delete<VesselType>(`${this.vesselTypesBaseUrl}/${id}`);
  }

  activateVesselType(id: string): Observable<VesselType> {
    return this.http.post<VesselType>(`${this.vesselTypesBaseUrl}/${id}/activate`, {});
  }
}
