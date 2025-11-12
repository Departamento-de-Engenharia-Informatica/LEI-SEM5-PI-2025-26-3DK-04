import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TranslationService } from '../../translation.service';
import { AdminService } from '../admin.service';

interface DockAssignment {
  dockId: string;
  distanceMeters: number;
}

// Tipagem precisa do enum StorageAreaType do backend
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
}

@Component({
  selector: 'app-manage-storage-areas',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './manageStorageAreas.html',
  styleUrls: ['./manageStorageAreas.scss']
})
export class ManageStorageAreas {
  storageAreas: StorageArea[] = [];
  docks: { id: string; name: string }[] = [];

  editing = false;
  editingId: string | null = null;

  storageForm: Omit<StorageArea, 'id' | 'currentOccupancyTEUs' | 'active'> = {
    code: '',
    designation: '',
    storageAreaType: 'Yard',
    coordinates: '',
    locationDescription: '',
    maxCapacityTEUs: 0,
    dockAssignments: []
  };

  constructor(private adminService: AdminService, private translation: TranslationService) {}

  t(key: string): string {
    return this.translation.translate(key);
  }

  ngOnInit() {
    this.loadStorageAreas();
    this.loadDocks();
  }

  loadStorageAreas() {
    this.adminService.getAllStorageAreas().subscribe({
      next: (res: StorageArea[]) => this.storageAreas = res,
      error: err => console.error('Error loading storage areas:', err)
    });
  }

  loadDocks() {
    this.adminService.getAllDocks().subscribe({
      next: (res: { id: string; name: string }[]) => this.docks = res,
      error: err => console.error('Error loading docks:', err)
    });
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
      }))
    };

    if (this.editing && this.editingId) {
      this.adminService.updateStorageArea(this.editingId, dto).subscribe({
        next: () => {
          this.loadStorageAreas();
          this.resetForm();
        },
        error: err => console.error('Error updating storage area:', err)
      });
    } else {
      this.adminService.createStorageArea(dto).subscribe({
        next: () => {
          this.loadStorageAreas();
          this.resetForm();
        },
        error: err => console.error('Error creating storage area:', err)
      });
    }
  }

  editStorageArea(area: StorageArea) {
    this.editing = true;
    this.editingId = area.id;
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
      }))
    };
  }

  softDeleteStorageArea(id: string) {
    if (confirm('Are you sure you want to delete this storage area?')) {
      this.adminService.inactivateStorageArea(id).subscribe(() => this.loadStorageAreas());
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
      dockAssignments: []
    };
  }

  onDockAssignmentChange(dockId: string, event: Event) {
    const target = event.target as HTMLInputElement;
    if (target.checked) {
      this.storageForm.dockAssignments.push({ dockId, distanceMeters: 0 });
    } else {
      this.storageForm.dockAssignments = this.storageForm.dockAssignments.filter(a => a.dockId !== dockId);
    }
  }

  updateDockDistance(dockId: string, event: Event) {
    const target = event.target as HTMLInputElement;
    const value = parseFloat(target.value);
    const assignment = this.storageForm.dockAssignments.find(a => a.dockId === dockId);
    if (assignment && !isNaN(value)) assignment.distanceMeters = value;
  }

  isDockAssigned(dockId: string): boolean {
    return this.storageForm.dockAssignments.some(a => a.dockId === dockId);
  }

  getDockDistance(dockId: string): number {
    return this.storageForm.dockAssignments.find(a => a.dockId === dockId)?.distanceMeters ?? 0;
  }
}
