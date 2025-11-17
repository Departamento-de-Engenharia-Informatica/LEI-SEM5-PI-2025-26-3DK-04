import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TranslationService } from '../../translation.service';
import { AdminService } from '../../admin/admin.service';
import { PortAuthorityOfficerService, Vessel, CreateVesselDto, UpdateVesselDto } from '../port-authority-officer.service';

interface VesselType {
  id: string;
  name: string;
}

@Component({
  selector: 'app-vessels',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './vessels.html',
  styleUrls: ['./vessels.scss']
})
export class Vessels implements OnInit {
  vessels: Vessel[] = [];
  vesselTypes: VesselType[] = [];

  editing = false;
  editingId: string | null = null;

  // Filters
  filters = {
    search: '',
    name: '',
    owner: '',
    operator: ''
  };

  vesselForm = {
    imoNumber: '',
    name: '',
    vesselTypeId: '',
    owner: '',
    operator: ''
  };

  constructor(
    private portAuthorityService: PortAuthorityOfficerService,
    private adminService: AdminService,
    private translation: TranslationService,
    private cdr: ChangeDetectorRef
  ) {}

  t(key: string) {
    return this.translation.translate(key);
  }

  ngOnInit() {
    console.log('Vessels: ngOnInit called');
    this.loadVesselTypes();
    this.loadVessels();
  }

  loadVessels() {
    console.log('Vessels: loadVessels called');
    this.portAuthorityService.getAllVessels().subscribe({
      next: res => {
        console.log('Vessels: received vessels', res);
        this.vessels = res;
        this.cdr.detectChanges();
      },
      error: err => {
        console.error('Error loading vessels:', err);
        const errorMessage = err?.error?.Message || err?.error?.message || 'Error loading vessels';
        alert('Error: ' + errorMessage);
      }
    });
  }

  loadVesselTypes() {
    console.log('Vessels: loadVesselTypes called');
    this.adminService.getVesselTypes().subscribe({
      next: res => {
        console.log('Vessels: received vessel types', res);
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
    console.log('Vessels: applying filters', this.filters);

    if (this.filters.search) {
      this.portAuthorityService.getAllVessels(this.filters.search).subscribe({
        next: res => {
          this.vessels = res;
          this.cdr.detectChanges();
        },
        error: err => {
          console.error('Error searching vessels:', err);
          alert('Error: ' + (err?.error?.Message || err?.error?.message || 'Error searching vessels'));
        }
      });
    } else if (this.filters.name) {
      this.portAuthorityService.searchVesselsByName(this.filters.name).subscribe({
        next: res => {
          this.vessels = res;
          this.cdr.detectChanges();
        },
        error: err => {
          console.error('Error searching by name:', err);
          alert('Error: ' + (err?.error?.Message || err?.error?.message || 'Error searching by name'));
        }
      });
    } else if (this.filters.owner) {
      this.portAuthorityService.searchVesselsByOwner(this.filters.owner).subscribe({
        next: res => {
          this.vessels = res;
          this.cdr.detectChanges();
        },
        error: err => {
          console.error('Error searching by owner:', err);
          alert('Error: ' + (err?.error?.Message || err?.error?.message || 'Error searching by owner'));
        }
      });
    } else if (this.filters.operator) {
      this.portAuthorityService.searchVesselsByOperator(this.filters.operator).subscribe({
        next: res => {
          this.vessels = res;
          this.cdr.detectChanges();
        },
        error: err => {
          console.error('Error searching by operator:', err);
          alert('Error: ' + (err?.error?.Message || err?.error?.message || 'Error searching by operator'));
        }
      });
    } else {
      this.loadVessels();
    }
  }

  clearFilters() {
    this.filters = {
      search: '',
      name: '',
      owner: '',
      operator: ''
    };
    this.loadVessels();
  }

  getStatusClass(active: boolean): string {
    return active ? 'status-active' : 'status-inactive';
  }

  editVessel(vessel: Vessel) {
    this.editing = true;
    this.editingId = vessel.id;
    const imoWithoutPrefix = vessel.imoNumber.startsWith('IMO') 
      ? vessel.imoNumber.substring(3) 
      : vessel.imoNumber;
    this.vesselForm = {
      imoNumber: imoWithoutPrefix,
      name: vessel.name,
      vesselTypeId: vessel.vesselTypeId,
      owner: vessel.owner,
      operator: vessel.operator
    };
    window.scrollTo({ top: 0, behavior: 'smooth' });
  }

  deactivateVessel(id: string) {
    if (!confirm(this.t('manageVessels.confirmDeactivate') || 'Are you sure you want to deactivate this vessel?')) {
      return;
    }

    this.portAuthorityService.inactivateVessel(id).subscribe({
      next: () => {
        alert(this.t('manageVessels.deactivateSuccess') || 'Vessel deactivated successfully');
        this.loadVessels();
      },
      error: err => {
        console.error('Error deactivating vessel:', err);
        alert('Error: ' + (err?.error?.Message || err?.error?.message || 'Error deactivating vessel'));
      }
    });
  }

  reactivateVessel(id: string) {
    if (!confirm(this.t('manageVessels.confirmReactivate') || 'Are you sure you want to reactivate this vessel?')) {
      return;
    }

    this.portAuthorityService.activateVessel(id).subscribe({
      next: () => {
        alert(this.t('manageVessels.reactivateSuccess') || 'Vessel reactivated successfully');
        this.loadVessels();
      },
      error: err => {
        console.error('Error reactivating vessel:', err);
        alert('Error: ' + (err?.error?.Message || err?.error?.message || 'Error reactivating vessel'));
      }
    });
  }

  saveVessel() {
    if (!this.validateForm()) {
      return;
    }

    // Add IMO prefix to the number before sending to backend
    const imoNumberWithPrefix = `IMO${this.vesselForm.imoNumber}`;

    if (this.editing && this.editingId) {
      const updateDto: UpdateVesselDto = {
        imoNumber: imoNumberWithPrefix,
        name: this.vesselForm.name,
        vesselTypeId: this.vesselForm.vesselTypeId,
        owner: this.vesselForm.owner,
        operator: this.vesselForm.operator
      };

      this.portAuthorityService.updateVessel(this.editingId, updateDto).subscribe({
        next: () => {
          alert(this.t('manageVessels.updateSuccess') || 'Vessel updated successfully');
          this.resetForm();
          this.loadVessels();
        },
        error: err => {
          console.error('Error updating vessel:', err);
          alert('Error: ' + (err?.error?.Message || err?.error?.message || 'Error updating vessel'));
        }
      });
    } else {
      const createDto: CreateVesselDto = {
        imoNumber: imoNumberWithPrefix,
        name: this.vesselForm.name,
        vesselTypeId: this.vesselForm.vesselTypeId,
        owner: this.vesselForm.owner,
        operator: this.vesselForm.operator
      };

      this.portAuthorityService.createVessel(createDto).subscribe({
        next: () => {
          alert(this.t('manageVessels.createSuccess') || 'Vessel created successfully');
          this.resetForm();
          this.loadVessels();
        },
        error: err => {
          console.error('Error creating vessel:', err);
          alert('Error: ' + (err?.error?.Message || err?.error?.message || 'Error creating vessel'));
        }
      });
    }
  }

  validateForm(): boolean {
    if (!this.vesselForm.imoNumber || !this.vesselForm.imoNumber.trim()) {
      alert(this.t('manageVessels.imoRequired') || 'IMO Number is required');
      return false;
    }

    if (!/^\d{7}$/.test(this.vesselForm.imoNumber)) {
      alert(this.t('manageVessels.imoInvalid') || 'IMO Number must be 7 digits');
      return false;
    }

    if (!this.vesselForm.name || !this.vesselForm.name.trim()) {
      alert(this.t('manageVessels.nameRequired') || 'Name is required');
      return false;
    }

    if (!this.vesselForm.vesselTypeId) {
      alert(this.t('manageVessels.vesselTypeRequired') || 'Vessel Type is required');
      return false;
    }

    if (!this.vesselForm.owner || !this.vesselForm.owner.trim()) {
      alert(this.t('manageVessels.ownerRequired') || 'Owner is required');
      return false;
    }

    if (!this.vesselForm.operator || !this.vesselForm.operator.trim()) {
      alert(this.t('manageVessels.operatorRequired') || 'Operator is required');
      return false;
    }

    return true;
  }

  resetForm() {
    this.editing = false;
    this.editingId = null;
    this.vesselForm = {
      imoNumber: '',
      name: '',
      vesselTypeId: '',
      owner: '',
      operator: ''
    };
  }
}
