import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { HttpClient, HttpParams } from '@angular/common/http';
import { AdminService } from '../admin.service';
import { TranslationService } from '../../translation.service';

@Component({
  selector: 'app-manage-physical-resources',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './managePhysicalResources.html',
  styleUrls: ['./managePhysicalResources.scss']
})
export class ManagePhysicalResources implements OnInit {
  resources: any[] = [];
  allResources: any[] = [];

  qualifications: any[] = [];

  filterDescription = '';
  filterType = '';

  editing = false;
  editingId: string | null = null;

  resourceForm: any = {
    description: '',
    type: '',
    capacity: 0,
    assignedArea: '',
    setupTime: null as number | null,
    status: '',
    qualificationIds: [] as { value: string }[]
  };

  statusOptions = ['Active', 'Inactive', 'Maintenance'];
  statusModalOpen = false;
  statusTarget: any = null;
  statusSelected: string = this.statusOptions[0];

  constructor(
    private http: HttpClient,
    private adminService: AdminService,
    private translation: TranslationService,
    private cdr: ChangeDetectorRef
  ) {}

  t(key: string) {
    return this.translation.translate(key);
  }

  ngOnInit() {
    this.loadQualifications();
    this.loadResources();
  }

  loadQualifications() {
    this.adminService.getAllQualifications().subscribe({
      next: res => {
        this.qualifications = res.map((q: any) => ({
          id: q.id ?? q.Id,
          name: q.name ?? q.Name
        }));
        this.cdr.detectChanges();
      },
      error: err => {
        alert('Error loading qualifications: ' + (err?.error?.message || err?.message));
      }
    });
  }

  loadResources() {
    let params = new HttpParams();
    if (this.filterDescription) params = params.set('description', this.filterDescription);
    if (this.filterType) params = params.set('type', this.filterType);

    this.adminService.getAllPhysicalResources().subscribe({
      next: res => {
        this.allResources = res.map(r => ({
          id: r.id ?? r.Id,
          description: r.description ?? r.Description,
          type: r.type ?? r.Type,
          capacity: r.capacity ?? r.Capacity,
          assignedArea: r.assignedArea ?? r.AssignedArea,
          setupTime: r.setupTime ?? r.SetupTime,
          status: r.status ?? r.Status,
          qualificationIds: r.qualificationIds ?? r.QualificationIds ?? []
        }));
        this.resources = [...this.allResources];
        this.cdr.detectChanges();
      },
      error: err => {
        alert('Error loading resources: ' + (err?.error?.Message || err?.error?.message));
      }
    });
  }

  applyFilter() {
    if (!this.filterDescription && !this.filterType) {
      this.resources = this.allResources;
      this.cdr.detectChanges();
      return;
    }

    this.resources = this.allResources.filter(r =>
      (!this.filterDescription ||
        r.description.toLowerCase().includes(this.filterDescription.toLowerCase())) &&
      (!this.filterType ||
        (r.type && r.type.toLowerCase().includes(this.filterType.toLowerCase())))
    );
    this.cdr.detectChanges();
  }

  clearFilter() {
    this.filterDescription = '';
    this.filterType = '';
    this.resources = this.allResources;
    this.cdr.detectChanges();
  }

  saveResource() {
    if (this.editing && this.editingId) {
      const dto = {
        Description: this.resourceForm.description,
        Capacity: Number(this.resourceForm.capacity),
        AssignedArea: this.resourceForm.assignedArea,
        SetupTime: this.resourceForm.setupTime,
        QualificationIds: this.resourceForm.qualificationIds
      };

      this.adminService.updatePhysicalResource(this.editingId, dto).subscribe({
        next: () => {
          alert('Resource updated successfully!');
          this.loadResources();
          this.resetForm();
        },
        error: err => alert('Error: ' + (err?.error?.message || JSON.stringify(err)))
      });

    } else {
      const dto = {
        Description: this.resourceForm.description,
        Type: this.resourceForm.type,
        Capacity: Number(this.resourceForm.capacity),
        AssignedArea: this.resourceForm.assignedArea,
        SetupTime: this.resourceForm.setupTime,
        Status: this.resourceForm.status,
        QualificationIds: this.resourceForm.qualificationIds
      };

      this.adminService.createPhysicalResource(dto).subscribe({
        next: () => {
          alert('Resource created successfully!');
          this.loadResources();
          this.resetForm();
        },
        error: err => alert('Error: ' + (err?.error?.message || JSON.stringify(err)))
      });
    }
  }

  editResource(r: any) {
    this.editing = true;
    this.editingId = r.id;

    this.resourceForm = {
      description: r.description,
      type: r.type,
      capacity: r.capacity,
      assignedArea: r.assignedArea,
      setupTime: r.setupTime,
      status: r.status,
      qualificationIds: r.qualificationIds.map((id: any) =>
        typeof id === 'string' ? { value: id } : id
      )
    };
    this.cdr.detectChanges();
  }

  resetForm() {
    this.editing = false;
    this.editingId = null;
    this.resourceForm = {
      description: '',
      type: '',
      capacity: 0,
      assignedArea: '',
      setupTime: null,
      status: this.statusOptions[0],
      qualificationIds: []
    };
    this.cdr.detectChanges();
  }

  onQualificationChange(event: any) {
    const id = event.target.value;

    if (event.target.checked) {
      // Adiciona objeto { value: id } se não existir
      if (!this.resourceForm.qualificationIds.some((q: any) => q.value === id)) {
        this.resourceForm.qualificationIds.push({ value: id });
      }
    } else {
      // Remove objeto pelo value
      this.resourceForm.qualificationIds =
        this.resourceForm.qualificationIds.filter((q: any) => q.value !== id);
    }
  }


  getQualificationName(id: string) {
    const q = this.qualifications.find(x => (x.id ?? x.Id) === id);
    return q ? (q.name ?? q.Name) : id;
  }

  openChangeStatusDialog(resource: any) {
    this.statusTarget = resource;
    this.statusSelected = resource.status ?? this.statusOptions[0];
    this.statusModalOpen = true;
    this.cdr.detectChanges();
  }

  closeStatusModal() {
    this.statusModalOpen = false;
    this.statusTarget = null;
    this.cdr.detectChanges();
  }

  confirmChangeStatus() {
    if (!this.statusTarget) return;

    this.adminService.updatePhysicalResourceStatus(
      this.statusTarget.id,
      this.statusSelected
    ).subscribe({
      next: () => {
        alert('Status changed successfully!');
        this.closeStatusModal();
        this.loadResources();
      },
      error: err => alert('Error: ' + (err?.error?.message || JSON.stringify(err)))
    });
  }
}
