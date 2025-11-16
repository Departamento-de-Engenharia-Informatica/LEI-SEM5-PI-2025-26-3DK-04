import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TranslationService } from '../../translation.service';
import { AdminService } from '../admin.service';

interface Qualification {
  id: string;
  name: string;
}

@Component({
  selector: 'app-manage-qualifications',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './manageQualifications.html',
  styleUrls: ['./manageQualifications.scss']
})
export class ManageQualifications implements OnInit {
  qualifications: Qualification[] = [];

  editing = false;
  editingId: string | null = null;

  // Filtros
  filterName: string = '';

  qualificationForm = {
    name: ''
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
    console.log('ManageQualifications: ngOnInit called');
    this.loadQualifications();
  }

  loadQualifications() {
    console.log('ManageQualifications: loadQualifications called');
    this.adminService.getAllQualifications().subscribe({
      next: res => {
        console.log('ManageQualifications: received qualifications', res);
        this.qualifications = res;
        this.cdr.detectChanges();
      },
      error: err => {
        console.error('Error loading qualifications:', err);
        const errorMessage = err?.error?.Message || err?.error?.message || 'Error loading qualifications';
        alert('Error: ' + errorMessage);
      }
    });
  }

  applyFilter() {
    if (!this.filterName) {
      this.loadQualifications();
      return;
    }

    console.log('ManageQualifications: applying filter', this.filterName);
    // Filtrar localmente se não houver endpoint de search no backend
    this.adminService.getAllQualifications().subscribe({
      next: res => {
        this.qualifications = res.filter(q => 
          q.name.toLowerCase().includes(this.filterName.toLowerCase())
        );
        this.cdr.detectChanges();
      },
      error: err => {
        console.error('Error filtering qualifications:', err);
        const errorMessage = err?.error?.Message || err?.error?.message || 'Error filtering qualifications';
        alert('Error: ' + errorMessage);
      }
    });
  }

  clearFilter() {
    this.filterName = '';
    this.loadQualifications();
  }

  saveQualification() {
    const dto = {
      Name: this.qualificationForm.name
    };

    if (this.editing && this.editingId) {
      this.adminService.updateQualification(this.editingId, dto).subscribe({
        next: () => {
          alert('Qualification updated successfully!');
          this.loadQualifications();
          this.resetForm();
        },
        error: err => {
          console.error('Error updating qualification:', err);
          const errorMessage = err?.error?.Message || err?.error?.message || 'Error updating qualification';
          alert('Error: ' + errorMessage);
        }
      });
    } else {
      this.adminService.createQualification(dto).subscribe({
        next: () => {
          alert('Qualification created successfully!');
          this.loadQualifications();
          this.resetForm();
        },
        error: err => {
          console.error('Error creating qualification:', err);
          const errorMessage = err?.error?.Message || err?.error?.message || 'Error creating qualification';
          alert('Error: ' + errorMessage);
        }
      });
    }
  }

  editQualification(qualification: Qualification) {
    this.editing = true;
    this.editingId = qualification.id;

    this.qualificationForm = {
      name: qualification.name
    };
  }

  resetForm() {
    this.editing = false;
    this.editingId = null;
    this.qualificationForm = {
      name: ''
    };
  }

  deleteQualification(id: string) {
    if (confirm('Are you sure you want to delete this qualification?')) {
      this.adminService.deleteQualification(id).subscribe({
        next: () => {
          alert('Qualification deleted successfully!');
          this.loadQualifications();
        },
        error: err => {
          console.error('Error deleting qualification:', err);
          const errorMessage = err?.error?.Message || err?.error?.message || 'Error deleting qualification';
          alert('Error: ' + errorMessage);
        }
      });
    }
  }
}
