import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TranslationService } from '../../translation.service';
import { PortAuthorityOfficerService, VesselType, CreateVesselTypeDto, UpdateVesselTypeDto } from '../port-authority-officer.service';

@Component({
  selector: 'app-vessel-types',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './vessel-types.html',
  styleUrls: ['./vessel-types.scss']
})
export class VesselTypes implements OnInit {
  vesselTypes: VesselType[] = [];

  editing = false;
  editingId: string | null = null;

  // Filters
  filters = {
    search: '',
    name: '',
    description: ''
  };

  vesselTypeForm = {
    name: '',
    description: '',
    capacity: 0,
    maxRows: 0,
    maxBays: 0,
    maxTiers: 0
  };

  constructor(
    private portAuthorityService: PortAuthorityOfficerService,
    private translation: TranslationService,
    private cdr: ChangeDetectorRef
  ) {}

  t(key: string) {
    return this.translation.translate(key);
  }

  ngOnInit() {
    console.log('VesselTypes: ngOnInit called');
    this.loadVesselTypes();
  }

  loadVesselTypes() {
    console.log('VesselTypes: loadVesselTypes called');
    this.portAuthorityService.getAllVesselTypes().subscribe({
      next: res => {
        console.log('VesselTypes: received vessel types', res);
        this.vesselTypes = res;
        this.cdr.detectChanges();
      },
      error: err => {
        console.error('Error loading vessel types:', err);
        const errorMessage = err?.error?.Message || err?.error?.message || 'Error loading vessel types';
        alert('Error: ' + errorMessage);
      }
    });
  }

  applyFilters() {
    console.log('VesselTypes: applying filters', this.filters);

    if (this.filters.search) {
      this.portAuthorityService.getAllVesselTypes(this.filters.search).subscribe({
        next: res => {
          this.vesselTypes = res;
          this.cdr.detectChanges();
        },
        error: err => {
          console.error('Error searching vessel types:', err);
          alert('Error: ' + (err?.error?.Message || err?.error?.message || 'Error searching vessel types'));
        }
      });
    } else if (this.filters.name) {
      this.portAuthorityService.searchVesselTypesByName(this.filters.name).subscribe({
        next: res => {
          this.vesselTypes = res;
          this.cdr.detectChanges();
        },
        error: err => {
          console.error('Error searching by name:', err);
          alert('Error: ' + (err?.error?.Message || err?.error?.message || 'Error searching by name'));
        }
      });
    } else if (this.filters.description) {
      this.portAuthorityService.searchVesselTypesByDescription(this.filters.description).subscribe({
        next: res => {
          this.vesselTypes = res;
          this.cdr.detectChanges();
        },
        error: err => {
          console.error('Error searching by description:', err);
          alert('Error: ' + (err?.error?.Message || err?.error?.message || 'Error searching by description'));
        }
      });
    } else {
      this.loadVesselTypes();
    }
  }

  clearFilters() {
    this.filters = {
      search: '',
      name: '',
      description: ''
    };
    this.loadVesselTypes();
  }

  getStatusClass(active: boolean): string {
    return active ? 'status-active' : 'status-inactive';
  }

  editVesselType(vesselType: VesselType) {
    this.editing = true;
    this.editingId = vesselType.id;
    this.vesselTypeForm = {
      name: vesselType.name,
      description: vesselType.description,
      capacity: vesselType.capacity,
      maxRows: vesselType.maxRows,
      maxBays: vesselType.maxBays,
      maxTiers: vesselType.maxTiers
    };
    window.scrollTo({ top: 0, behavior: 'smooth' });
  }

  deactivateVesselType(id: string) {
    if (!confirm(this.t('manageVesselTypes.confirmDeactivate') || 'Are you sure you want to deactivate this vessel type?')) {
      return;
    }

    this.portAuthorityService.inactivateVesselType(id).subscribe({
      next: () => {
        alert(this.t('manageVesselTypes.deactivateSuccess') || 'Vessel type deactivated successfully');
        this.loadVesselTypes();
      },
      error: err => {
        console.error('Error deactivating vessel type:', err);
        alert('Error: ' + (err?.error?.Message || err?.error?.message || 'Error deactivating vessel type'));
      }
    });
  }

  reactivateVesselType(id: string) {
    if (!confirm(this.t('manageVesselTypes.confirmReactivate') || 'Are you sure you want to reactivate this vessel type?')) {
      return;
    }

    this.portAuthorityService.activateVesselType(id).subscribe({
      next: () => {
        alert(this.t('manageVesselTypes.reactivateSuccess') || 'Vessel type reactivated successfully');
        this.loadVesselTypes();
      },
      error: err => {
        console.error('Error reactivating vessel type:', err);
        alert('Error: ' + (err?.error?.Message || err?.error?.message || 'Error reactivating vessel type'));
      }
    });
  }

  saveVesselType() {
    if (!this.validateForm()) {
      return;
    }

    if (this.editing && this.editingId) {
      const updateDto: UpdateVesselTypeDto = {
        name: this.vesselTypeForm.name,
        description: this.vesselTypeForm.description,
        capacity: this.vesselTypeForm.capacity,
        maxRows: this.vesselTypeForm.maxRows,
        maxBays: this.vesselTypeForm.maxBays,
        maxTiers: this.vesselTypeForm.maxTiers
      };

      this.portAuthorityService.updateVesselType(this.editingId, updateDto).subscribe({
        next: () => {
          alert(this.t('manageVesselTypes.updateSuccess') || 'Vessel type updated successfully');
          this.resetForm();
          this.loadVesselTypes();
        },
        error: err => {
          console.error('Error updating vessel type:', err);
          alert('Error: ' + (err?.error?.Message || err?.error?.message || 'Error updating vessel type'));
        }
      });
    } else {
      const createDto: CreateVesselTypeDto = {
        name: this.vesselTypeForm.name,
        description: this.vesselTypeForm.description,
        capacity: this.vesselTypeForm.capacity,
        maxRows: this.vesselTypeForm.maxRows,
        maxBays: this.vesselTypeForm.maxBays,
        maxTiers: this.vesselTypeForm.maxTiers
      };

      this.portAuthorityService.createVesselType(createDto).subscribe({
        next: () => {
          alert(this.t('manageVesselTypes.createSuccess') || 'Vessel type created successfully');
          this.resetForm();
          this.loadVesselTypes();
        },
        error: err => {
          console.error('Error creating vessel type:', err);
          alert('Error: ' + (err?.error?.Message || err?.error?.message || 'Error creating vessel type'));
        }
      });
    }
  }

  validateForm(): boolean {
    if (!this.vesselTypeForm.name || !this.vesselTypeForm.name.trim()) {
      alert(this.t('manageVesselTypes.nameRequired') || 'Name is required');
      return false;
    }

    if (!this.vesselTypeForm.description || !this.vesselTypeForm.description.trim()) {
      alert(this.t('manageVesselTypes.descriptionRequired') || 'Description is required');
      return false;
    }

    if (this.vesselTypeForm.capacity <= 0) {
      alert(this.t('manageVesselTypes.capacityInvalid') || 'Capacity must be greater than 0');
      return false;
    }

    if (this.vesselTypeForm.maxRows <= 0) {
      alert(this.t('manageVesselTypes.maxRowsInvalid') || 'Max Rows must be greater than 0');
      return false;
    }

    if (this.vesselTypeForm.maxBays <= 0) {
      alert(this.t('manageVesselTypes.maxBaysInvalid') || 'Max Bays must be greater than 0');
      return false;
    }

    if (this.vesselTypeForm.maxTiers <= 0) {
      alert(this.t('manageVesselTypes.maxTiersInvalid') || 'Max Tiers must be greater than 0');
      return false;
    }

    return true;
  }

  resetForm() {
    this.editing = false;
    this.editingId = null;
    this.vesselTypeForm = {
      name: '',
      description: '',
      capacity: 0,
      maxRows: 0,
      maxBays: 0,
      maxTiers: 0
    };
  }
}
