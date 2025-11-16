import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TranslationService } from '../../translation.service';
import { AdminService } from '../admin.service';

interface OperationalWindow {
  startTime: string;
  endTime: string;
}

type MemberStatus = 'Available' | 'Unavailable';

interface Qualification {
  id: string;
  name: string;
  description: string;
}

interface StaffMember {
  id: string;
  name: string;
  email: string;
  phoneNumber: number;
  operationalWindow: OperationalWindow;
  status: MemberStatus;
  qualifications: Qualification[];
}

@Component({
  selector: 'app-manage-staff-members',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './manageStaffMembers.html',
  styleUrls: ['./manageStaffMembers.scss']
})
export class ManageStaffMembers implements OnInit {
  staffMembers: StaffMember[] = [];
  qualifications: Qualification[] = [];

  editing = false;
  editingId: string | null = null;
  originalQualificationIds: string[] = []; // Para guardar qualificações originais ao editar

  // Filtros
  filters = {
    name: '',
    status: '',
    qualificationId: ''
  };

  staffForm = {
    name: '',
    email: '',
    phoneNumber: 0,
    startTime: '',
    endTime: '',
    status: 'Available' as MemberStatus,
    qualificationIds: [] as string[]
  };

  qualificationsMap: { [qualificationId: string]: { assigned: boolean } } = {};

  constructor(
    private adminService: AdminService, 
    private translation: TranslationService,
    private cdr: ChangeDetectorRef
  ) {}

  t(key: string) {
    return this.translation.translate(key);
  }

  ngOnInit() {
    console.log('ManageStaffMembers: ngOnInit called');
    // Carrega os dados automaticamente ao inicializar o componente
    this.loadQualifications();
    this.loadStaffMembers();
  }

  loadStaffMembers() {
    console.log('ManageStaffMembers: loadStaffMembers called');
    this.adminService.getAllStaffMembers().subscribe({
      next: res => {
        console.log('ManageStaffMembers: received staff members', res);
        this.staffMembers = res;
        this.cdr.detectChanges();
      },
      error: err => {
        console.error('Error loading staff members:', err);
        const errorMessage = err?.error?.Message || err?.error?.message || 'Error loading staff members';
        alert('Error: ' + errorMessage);
      }
    });
  }

  applyFilters() {
    console.log('ManageStaffMembers: applying filters', this.filters);
    
    // Se todos os filtros estão vazios, carregar todos
    if (!this.filters.name && !this.filters.status && !this.filters.qualificationId) {
      this.loadStaffMembers();
      return;
    }

    // Usar o endpoint de search com os filtros
    this.adminService.searchStaffMembers(
      this.filters.name || undefined,
      this.filters.status || undefined,
      this.filters.qualificationId || undefined
    ).subscribe({
      next: res => {
        console.log('ManageStaffMembers: search results', res);
        this.staffMembers = res;
        this.cdr.detectChanges();
      },
      error: err => {
        console.error('Error searching staff members:', err);
        const errorMessage = err?.error?.Message || err?.error?.message || 'Error searching staff members';
        alert('Error: ' + errorMessage);
      }
    });
  }

  clearFilters() {
    this.filters = {
      name: '',
      status: '',
      qualificationId: ''
    };
    this.loadStaffMembers();
  }

  loadQualifications() {
    console.log('ManageStaffMembers: loadQualifications called');
    this.adminService.getAllQualifications().subscribe({
      next: res => {
        console.log('ManageStaffMembers: received qualifications', res);
        this.qualifications = res;
        this.initQualificationsMap();
        this.cdr.detectChanges();
      },
      error: err => {
        console.error('Error loading qualifications:', err);
        const errorMessage = err?.error?.Message || err?.error?.message || 'Error loading qualifications';
        alert('Error: ' + errorMessage);
      }
    });
  }

  initQualificationsMap() {
    this.qualificationsMap = {};
    for (const qual of this.qualifications) {
      this.qualificationsMap[qual.id] = { assigned: false };
    }
  }

  saveStaffMember() {
    const dto = {
      Name: this.staffForm.name,
      Email: this.staffForm.email,
      PhoneNumber: this.staffForm.phoneNumber,
      OperationalWindow: {
        StartTime: this.staffForm.startTime,
        EndTime: this.staffForm.endTime
      },
      Status: this.staffForm.status
    };

    if (this.editing && this.editingId) {
      this.adminService.updateStaffMember(this.editingId, dto).subscribe({
        next: () => {
          // Sincronizar qualificações após atualizar o staff member
          this.syncQualifications(this.editingId!);
        },
        error: err => {
          console.error('Error updating staff member:', err);
          const errorMessage = err?.error?.Message || err?.error?.message || 'Error updating staff member';
          alert('Error: ' + errorMessage);
        }
      });
    } else {
      this.adminService.createStaffMember(dto).subscribe({
        next: (createdStaff) => {
          // Add qualifications after creation
          if (this.staffForm.qualificationIds.length > 0) {
            this.addQualificationsToStaff(createdStaff.id, this.staffForm.qualificationIds);
          }
          alert('Staff member created successfully!');
          this.loadStaffMembers();
          this.resetForm();
        },
        error: err => {
          console.error('Error creating staff member:', err);
          const errorMessage = err?.error?.Message || err?.error?.message || 'Error creating staff member';
          alert('Error: ' + errorMessage);
        }
      });
    }
  }

  addQualificationsToStaff(staffId: string, qualificationIds: string[]) {
    qualificationIds.forEach(qualId => {
      this.adminService.addQualificationToStaff(staffId, qualId).subscribe({
        error: err => console.error('Error adding qualification:', err)
      });
    });
  }

  editStaffMember(staff: StaffMember) {
    this.editing = true;
    this.editingId = staff.id;

    // Guardar IDs das qualificações originais
    this.originalQualificationIds = staff.qualifications.map(q => q.id);

    this.staffForm = {
      name: staff.name,
      email: staff.email,
      phoneNumber: staff.phoneNumber,
      startTime: staff.operationalWindow.startTime,
      endTime: staff.operationalWindow.endTime,
      status: staff.status,
      qualificationIds: [...this.originalQualificationIds] // Copiar array
    };

    // Update qualifications map
    for (const qual of this.qualifications) {
      const assigned = staff.qualifications.some(q => q.id === qual.id);
      this.qualificationsMap[qual.id] = { assigned };
    }
  }

  syncQualifications(staffId: string) {
    // Determinar quais qualificações foram adicionadas e removidas
    const currentIds = this.staffForm.qualificationIds;
    const toAdd = currentIds.filter(id => !this.originalQualificationIds.includes(id));
    const toRemove = this.originalQualificationIds.filter(id => !currentIds.includes(id));

    let operations = 0;
    let completed = 0;
    const totalOps = toAdd.length + toRemove.length;

    if (totalOps === 0) {
      // Nenhuma alteração nas qualificações
      alert('Staff member updated successfully!');
      this.loadStaffMembers();
      this.resetForm();
      return;
    }

    const checkComplete = () => {
      completed++;
      if (completed === totalOps) {
        alert('Staff member updated successfully!');
        this.loadStaffMembers();
        this.resetForm();
      }
    };

    // Adicionar novas qualificações
    toAdd.forEach(qualId => {
      this.adminService.addQualificationToStaff(staffId, qualId).subscribe({
        next: () => checkComplete(),
        error: err => {
          console.error('Error adding qualification:', err);
          checkComplete();
        }
      });
    });

    // Remover qualificações
    toRemove.forEach(qualId => {
      this.adminService.removeQualificationFromStaff(staffId, qualId).subscribe({
        next: () => checkComplete(),
        error: err => {
          console.error('Error removing qualification:', err);
          checkComplete();
        }
      });
    });
  }

  resetForm() {
    this.editing = false;
    this.editingId = null;
    this.originalQualificationIds = [];
    this.staffForm = {
      name: '',
      email: '',
      phoneNumber: 0,
      startTime: '',
      endTime: '',
      status: 'Available',
      qualificationIds: []
    };
    this.initQualificationsMap();
  }

  deactivateStaffMember(id: string) {
    if (confirm('Are you sure you want to deactivate this staff member?')) {
      this.adminService.deactivateStaffMember(id).subscribe({
        next: () => {
          alert('Staff member deactivated successfully!');
          this.loadStaffMembers();
        },
        error: err => {
          console.error('Error deactivating staff member:', err);
          const errorMessage = err?.error?.Message || err?.error?.message || 'Error deactivating staff member';
          alert('Error: ' + errorMessage);
        }
      });
    }
  }

  reactivateStaffMember(id: string) {
    if (confirm('Are you sure you want to reactivate this staff member?')) {
      this.adminService.reactivateStaffMember(id).subscribe({
        next: () => {
          alert('Staff member reactivated successfully!');
          this.loadStaffMembers();
        },
        error: err => {
          console.error('Error reactivating staff member:', err);
          const errorMessage = err?.error?.Message || err?.error?.message || 'Error reactivating staff member';
          alert('Error: ' + errorMessage);
        }
      });
    }
  }

  isQualificationAssigned(qualificationId: string): boolean {
    return !!this.qualificationsMap[qualificationId]?.assigned;
  }

  onQualificationChange(qualificationId: string, event: any) {
    const checked = event.target.checked;
    this.qualificationsMap[qualificationId].assigned = checked;

    if (checked) {
      if (!this.staffForm.qualificationIds.includes(qualificationId)) {
        this.staffForm.qualificationIds.push(qualificationId);
      }
    } else {
      this.staffForm.qualificationIds = this.staffForm.qualificationIds.filter(id => id !== qualificationId);
    }
  }

  addQualification(staffId: string, qualificationId: string) {
    this.adminService.addQualificationToStaff(staffId, qualificationId).subscribe({
      next: () => {
        alert('Qualification added successfully!');
        this.loadStaffMembers();
      },
      error: err => {
        console.error('Error adding qualification:', err);
        const errorMessage = err?.error?.Message || err?.error?.message || 'Error adding qualification';
        alert('Error: ' + errorMessage);
      }
    });
  }

  removeQualification(staffId: string, qualificationId: string) {
    if (confirm('Remove this qualification?')) {
      this.adminService.removeQualificationFromStaff(staffId, qualificationId).subscribe({
        next: () => {
          alert('Qualification removed successfully!');
          this.loadStaffMembers();
        },
        error: err => {
          console.error('Error removing qualification:', err);
          const errorMessage = err?.error?.Message || err?.error?.message || 'Error removing qualification';
          alert('Error: ' + errorMessage);
        }
      });
    }
  }

  getStatusClass(status: MemberStatus): string {
    return status === 'Available' ? 'status-available' : 'status-unavailable';
  }
}
