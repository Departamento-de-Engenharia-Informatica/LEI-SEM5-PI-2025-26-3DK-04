import { Component } from '@angular/core';
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

  storageForm = {
    code: '',
    designation: '',
    storageAreaType: 'Yard' as StorageAreaType,
    coordinates: '',
    locationDescription: '',
    maxCapacityTEUs: 0,
    dockAssignments: [] as DockAssignment[]
  };

  dockAssignmentsMap: { [dockId: string]: { assigned: boolean; distance: number } } = {};

  constructor(private adminService: AdminService, private translation: TranslationService) {}

  t(key: string) {
    return this.translation.translate(key);
  }

  ngOnInit() {
    this.loadDocks();
    this.loadStorageAreas();
  }

  loadStorageAreas() {
    this.adminService.getAllStorageAreas().subscribe({
      next: res => (this.storageAreas = res),
      error: err => console.error('Error loading storage areas:', err)
    });
  }

  loadDocks() {
    this.adminService.getAllDocks().subscribe({
      next: res => {
        this.docks = res;
        this.initDockAssignmentsMap();
      },
      error: err => console.error('Error loading docks:', err)
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
      }))
    };

    if (this.editing && this.editingId) {
      this.adminService.updateStorageArea(this.editingId, dto).subscribe(() => {
        this.loadStorageAreas();
        this.resetForm();
      });
    } else {
      this.adminService.createStorageArea(dto).subscribe(() => {
        this.loadStorageAreas();
        this.resetForm();
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
      }))
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
      dockAssignments: []
    };
    this.initDockAssignmentsMap();
  }

  softDeleteStorageArea(id: string) {
    if (confirm('Are you sure?')) {
      this.adminService.inactivateStorageArea(id).subscribe(() => this.loadStorageAreas());
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
