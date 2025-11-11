import { Component } from '@angular/core';
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
export class ManageDocks {
  docks: any[] = [];
  vesselTypes: any[] = [];

  editing = false;
  editingId: string | null = null;

  dockForm = {
    name: '',
    length: 0,
    depth: 0,
    maxDraft: 0,
    coordinates: '',
    locationDescription: '',
    vesselTypeIds: [] as string[]
  };

  constructor(private adminService: AdminService, private translation: TranslationService) {}

  t(key: string) {
    return this.translation.translate(key);
  }

  ngOnInit() {
    // chama os vessel types antes de qualquer outra coisa
    this.loadVesselTypes();

    // só depois carrega os docks
    this.loadDocks();
  }

  loadDocks() {
    this.adminService.getAllDocks().subscribe(res => this.docks = res);
  }

  loadVesselTypes() {
    this.adminService.getVesselTypes().subscribe({
      next: res => {
        this.vesselTypes = res.map(v => ({
          id: v.id ?? v.Id,
          name: v.name ?? v.Name
        }));
      },
      error: err => console.error('Erro ao carregar Vessel Types:', err)
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
      this.adminService.updateDock(this.editingId, dto).subscribe(() => {
        this.loadDocks();
        this.resetForm();
      });
    } else {
      this.adminService.createDock(dto).subscribe(() => {
        this.loadDocks();
        this.resetForm();
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
      this.adminService.softDeleteDock(id).subscribe(() => this.loadDocks());
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
