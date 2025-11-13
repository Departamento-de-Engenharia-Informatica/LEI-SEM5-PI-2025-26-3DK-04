import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { TranslationService } from '../../translation.service';
import { AdminService } from '../admin.service';

@Component({
  selector: 'app-manage-docks',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './manageDocks.html',
  styleUrls: ['./manageDocks.scss']
})
export class ManageDocks implements OnInit {
  docks: any[] = [];
  vesselTypes: any[] = [];

  editing = false;
  editingId: string | null = null;

  // Filtros
  filterName: string = '';
  allDocks: any[] = []; // Para manter todos os docks antes de filtrar

  dockForm = {
    name: '',
    length: 0,
    depth: 0,
    maxDraft: 0,
    coordinates: '',
    locationDescription: '',
    vesselTypeIds: [] as string[]
  };

  constructor(
    private adminService: AdminService, 
    private translation: TranslationService,
    private cdr: ChangeDetectorRef
  ) {}

  t(key: string) {
    return this.translation.translate(key);
  }

  ngOnInit() {
    console.log('ManageDocks: ngOnInit called');
    // chama os vessel types antes de qualquer outra coisa
    this.loadVesselTypes();

    // só depois carrega os docks
    this.loadDocks();
  }

  loadDocks() {
    console.log('ManageDocks: loadDocks called');
    this.adminService.getAllDocks().subscribe({
      next: res => {
        console.log('ManageDocks: received docks', res);
        this.allDocks = res;
        this.docks = res;
        this.cdr.detectChanges();
      },
      error: err => {
        console.error('Error loading docks:', err);
        const errorMessage = err?.error?.Message || err?.error?.message || 'Error loading docks';
        alert('Error: ' + errorMessage);
      }
    });
  }

  applyFilter() {
    if (!this.filterName) {
      this.docks = this.allDocks;
      this.cdr.detectChanges();
      return;
    }

    console.log('ManageDocks: applying filter', this.filterName);
    this.docks = this.allDocks.filter(d => 
      d.name.toLowerCase().includes(this.filterName.toLowerCase())
    );
    this.cdr.detectChanges();
  }

  clearFilter() {
    this.filterName = '';
    this.docks = this.allDocks;
    this.cdr.detectChanges();
  }

  loadVesselTypes() {
    console.log('ManageDocks: loadVesselTypes called');
    this.adminService.getVesselTypes().subscribe({
      next: res => {
        console.log('ManageDocks: received vessel types', res);
        this.vesselTypes = res.map(v => ({
          id: v.id ?? v.Id,
          name: v.name ?? v.Name
        }));
        this.cdr.detectChanges();
      },
      error: err => {
        console.error('Error loading vessel types:', err);
        const errorMessage = err?.error?.Message || err?.error?.message || 'Error loading vessel types';
        alert('Error: ' + errorMessage);
      }
    });
  }

  saveDock() {
    const dto = {
      Name: this.dockForm.name,
      Length: this.dockForm.length,
      Depth: this.dockForm.depth,
      MaxDraft: this.dockForm.maxDraft,
      Coordinates: this.dockForm.coordinates,
      LocationDescription: this.dockForm.locationDescription,
      VesselTypeIds: this.dockForm.vesselTypeIds.map(id => id)
    };

    if (this.editing && this.editingId) {
      this.adminService.updateDock(this.editingId, dto).subscribe({
        next: () => {
          alert('Dock updated successfully!');
          this.loadDocks();
          this.resetForm();
        },
        error: err => {
          console.error('Error updating dock:', err);
          const errorMessage = err?.error?.Message || err?.error?.message || 'Error updating dock';
          alert('Error: ' + errorMessage);
        }
      });
    } else {
      this.adminService.createDock(dto).subscribe({
        next: () => {
          alert('Dock created successfully!');
          this.loadDocks();
          this.resetForm();
        },
        error: err => {
          console.error('Error creating dock:', err);
          const errorMessage = err?.error?.Message || err?.error?.message || 'Error creating dock';
          alert('Error: ' + errorMessage);
        }
      });
    }
  }

  editDock(d: any) {
    this.editing = true;
    this.editingId = d.id;

    // Prefere os ids se existirem, senão tenta outras propriedades
    const ids = d.allowedVesselTypeIds ?? d.vesselTypeIds ?? [];

    this.dockForm = {
      name: d.name,
      length: d.length,
      depth: d.depth,
      maxDraft: d.maxDraft,
      coordinates: d.coordinates,
      locationDescription: d.locationDescription,
      vesselTypeIds: ids.map((id: any) => id?.toString ? id.toString() : id)
    };
  }


  softDeleteDock(id: string) {
    if (confirm('Are you sure?')) {
      this.adminService.softDeleteDock(id).subscribe({
        next: () => {
          alert('Dock deleted successfully!');
          this.loadDocks();
        },
        error: err => {
          console.error('Error deleting dock:', err);
          const errorMessage = err?.error?.Message || err?.error?.message || 'Error deleting dock';
          alert('Error: ' + errorMessage);
        }
      });
    }
  }

  resetForm() {
    this.editing = false;
    this.editingId = null;
    this.dockForm = {
      name: '',
      length: 0,
      depth: 0,
      maxDraft: 0,
      coordinates: '',
      locationDescription: '',
      vesselTypeIds: []
    };
  }

  onVesselTypeChange(event: any) {
    const id = event.target.value;
    if (event.target.checked) {
      if (!this.dockForm.vesselTypeIds.includes(id)) {
        this.dockForm.vesselTypeIds.push(id);
      }
    } else {
      this.dockForm.vesselTypeIds = this.dockForm.vesselTypeIds.filter(v => v !== id);
    }
  }

}
