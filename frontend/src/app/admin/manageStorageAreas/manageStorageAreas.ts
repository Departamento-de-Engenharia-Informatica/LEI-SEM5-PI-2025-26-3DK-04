import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TranslationService } from '../../translation.service';
import { AdminService } from '../admin.service';

interface DockAssignment {
  dockId: string;
  distanceMeters: number;
}

type StorageAreaType = 'Yard' | 'Warehouse' | 'Refrigerated' | 'Other';

interface StorageArea {
  id: string;
  code: string;
  designation: string;
  storageAreaType: StorageAreaType;
  coordinates: string;
  locationDescription: string;
  maxCapacityTEUs: number;
  currentOccupancyTEUs: number;
  active: boolean;
  dockAssignments: DockAssignment[];
  length: number;
  width: number;
  height: number;
}

@Component({
  selector: 'app-manage-storage-areas',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './manageStorageAreas.html',
  styleUrls: ['./manageStorageAreas.scss']
})
export class ManageStorageAreas implements OnInit {
  storageAreas: StorageArea[] = [];
  docks: { id: string; name: string }[] = [];

  editing = false;
  editingId: string | null = null;

  // Filtros
  filterName: string = '';
  allStorageAreas: StorageArea[] = []; // Para manter todas as storage areas antes de filtrar

  storageForm = {
    code: '',
    designation: '',
    storageAreaType: 'Yard' as StorageAreaType,
    coordinates: '',
    locationDescription: '',
    maxCapacityTEUs: 0,
    dockAssignments: [] as DockAssignment[],
    length: 0,
    width: 0,
    height: 0
  };

  dockAssignmentsMap: { [dockId: string]: { assigned: boolean; distance: number } } = {};

  constructor(
    private adminService: AdminService,
    private translation: TranslationService,
    private cdr: ChangeDetectorRef
  ) {}

  t(key: string) {
    return this.translation.translate(key);
  }

  ngOnInit() {
    console.log('ManageStorageAreas: ngOnInit called');
    this.loadDocks();
    this.loadStorageAreas();
  }

  loadStorageAreas() {
    console.log('ManageStorageAreas: loadStorageAreas called');
    this.adminService.getAllStorageAreas().subscribe({
      next: res => {
        console.log('ManageStorageAreas: received storage areas', res);
        this.allStorageAreas = res;
        this.storageAreas = res;
        this.cdr.detectChanges();
      },
      error: err => {
        console.error('Error loading storage areas:', err);
        const errorMessage = err?.error?.Message || err?.error?.message || 'Error loading storage areas';
        alert('Error: ' + errorMessage);
      }
    });
  }

  applyFilter() {
    if (!this.filterName) {
      this.storageAreas = this.allStorageAreas;
      this.cdr.detectChanges();
      return;
    }

    console.log('ManageStorageAreas: applying filter', this.filterName);
    this.storageAreas = this.allStorageAreas.filter(sa =>
      sa.designation.toLowerCase().includes(this.filterName.toLowerCase()) ||
      sa.code.toLowerCase().includes(this.filterName.toLowerCase())
    );
    this.cdr.detectChanges();
  }

  clearFilter() {
    this.filterName = '';
    this.storageAreas = this.allStorageAreas;
    this.cdr.detectChanges();
  }

  loadDocks() {
    console.log('ManageStorageAreas: loadDocks called');
    this.adminService.getAllDocks().subscribe({
      next: res => {
        console.log('ManageStorageAreas: received docks', res);
        this.docks = res;
        this.initDockAssignmentsMap();
        this.cdr.detectChanges();
      },
      error: err => {
        console.error('Error loading docks:', err);
        const errorMessage = err?.error?.Message || err?.error?.message || 'Error loading docks';
        alert('Error: ' + errorMessage);
      }
    });
  }

  initDockAssignmentsMap() {
    this.dockAssignmentsMap = {};
    for (const dock of this.docks) {
      this.dockAssignmentsMap[dock.id] = { assigned: false, distance: 0 };
    }
  }

  saveStorageArea() {
    const dto = {
      Code: this.storageForm.code,
      Designation: this.storageForm.designation,
      StorageAreaType: this.storageForm.storageAreaType,
      Coordinates: this.storageForm.coordinates,
      LocationDescription: this.storageForm.locationDescription,
      MaxCapacityTEUs: this.storageForm.maxCapacityTEUs,
      InitialDockAssignments: this.storageForm.dockAssignments.map(a => ({
        DockId: a.dockId,
        DistanceMeters: a.distanceMeters
      })),
      Length: this.storageForm.length,
      Width: this.storageForm.width,
      Height: this.storageForm.height
    };

    if (this.editing && this.editingId) {
      this.adminService.updateStorageArea(this.editingId, dto).subscribe({
        next: () => {
          alert('Storage area updated successfully!');
          this.loadStorageAreas();
          this.resetForm();
        },
        error: err => {
          console.error('Error updating storage area:', err);
          const errorMessage = err?.error?.Message || err?.error?.message || 'Error updating storage area';
          alert('Error: ' + errorMessage);
        }
      });
    } else {
      this.adminService.createStorageArea(dto).subscribe({
        next: () => {
          alert('Storage area created successfully!');
          this.loadStorageAreas();
          this.resetForm();
        },
        error: err => {
          console.error('Error creating storage area:', err);
          const errorMessage = err?.error?.Message || err?.error?.message || 'Error creating storage area';
          alert('Error: ' + errorMessage);
        }
      });
    }
  }

  editStorageArea(area: StorageArea) {
    this.editing = true;
    this.editingId = area.id;

    // Atualiza o formulário com os dados existentes
    this.storageForm = {
      code: area.code,
      designation: area.designation,
      storageAreaType: area.storageAreaType,
      coordinates: area.coordinates,
      locationDescription: area.locationDescription,
      maxCapacityTEUs: area.maxCapacityTEUs,
      dockAssignments: area.dockAssignments.map(a => ({
        dockId: a.dockId,
        distanceMeters: a.distanceMeters
      })),
      length: area.length,
      width: area.width,
      height: area.height
    };

    // Atualiza o mapa de docks com os dados do storage
    for (const dock of this.docks) {
      const assigned = area.dockAssignments.find(a => a.dockId === dock.id);
      this.dockAssignmentsMap[dock.id] = {
        assigned: !!assigned,
        distance: assigned ? assigned.distanceMeters : 0
      };
    }
  }

  resetForm() {
    this.editing = false;
    this.editingId = null;
    this.storageForm = {
      code: '',
      designation: '',
      storageAreaType: 'Yard',
      coordinates: '',
      locationDescription: '',
      maxCapacityTEUs: 0,
      dockAssignments: [],
      length: 0,
      width: 0,
      height: 0
    };
    this.initDockAssignmentsMap();
  }

  softDeleteStorageArea(id: string) {
    if (confirm('Are you sure?')) {
      this.adminService.inactivateStorageArea(id).subscribe({
        next: () => {
          alert('Storage area inactivated successfully!');
          this.loadStorageAreas();
        },
        error: err => {
          console.error('Error inactivating storage area:', err);
          const errorMessage = err?.error?.Message || err?.error?.message || 'Error inactivating storage area';
          alert('Error: ' + errorMessage);
        }
      });
    }
  }

  isDockAssigned(dockId: string): boolean {
    return !!this.dockAssignmentsMap[dockId]?.assigned;
  }

  getDockDistance(dockId: string): number {
    return this.dockAssignmentsMap[dockId]?.distance ?? 0;
  }

  onDockAssignmentChange(dockId: string, event: any) {
    const checked = event.target.checked;
    this.dockAssignmentsMap[dockId].assigned = checked;

    if (checked) {
      if (!this.storageForm.dockAssignments.some(a => a.dockId === dockId)) {
        this.storageForm.dockAssignments.push({ dockId, distanceMeters: 0 });
      }
    } else {
      this.storageForm.dockAssignments = this.storageForm.dockAssignments.filter(a => a.dockId !== dockId);
    }
  }

  updateDockDistance(dockId: string, event: any) {
    const val = parseFloat(event.target.value);
    this.dockAssignmentsMap[dockId].distance = val;
    const assignment = this.storageForm.dockAssignments.find(a => a.dockId === dockId);
    if (assignment) assignment.distanceMeters = val;
  }
}
