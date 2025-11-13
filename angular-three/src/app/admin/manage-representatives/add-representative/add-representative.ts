import { Component, Inject, PLATFORM_ID } from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TranslationService } from '../../../translation.service';
import { AdminService } from '../../admin.service';
import { Router } from '@angular/router';

@Component({
  selector: 'app-add-representative',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './add-representative.html',
  styleUrls: ['./add-representative.scss']
})
export class AddRepresentative {
  repForm = {
    name: '',
    citizenId: '',
    nationality: '',
    email: '',
    phoneNumber: '',
    organizationId: ''
  };

  organizations: any[] = [];
  filteredOrganizations: any[] = [];
  searchTerm = '';
  currentLang = 'en';

  constructor(
    private adminService: AdminService,
    private translation: TranslationService,
    @Inject(PLATFORM_ID) private platformId: Object,
    private router: Router
  ) {
    const isBrowser = isPlatformBrowser(this.platformId);
    if (isBrowser) {
      const savedLang = localStorage.getItem('appLang');
      if (savedLang) this.translation.setLanguage(savedLang);
      this.currentLang = savedLang || 'en';
    }
  }

  t(key: string) {
    return this.translation.translate(key);
  }

  ngOnInit() {
    this.loadOrganizations();
  }

  loadOrganizations() {
    this.adminService.getAllOrganizations().subscribe({
      next: res => {
        this.organizations = res;
        this.applyFilter();
      },
      error: err => {
        console.error(err);
        alert(this.t('manageRepresentatives.loadOrganizationsError'));
      }
    });
  }

  applyFilter() {
    const q = (this.searchTerm || '').toLowerCase();
    this.filteredOrganizations = this.organizations.filter(org =>
      (org.legalName || '').toLowerCase().includes(q) ||
      (org.id || '').toLowerCase().includes(q)
    );
  }

  saveRepresentative() {
    if (!this.repForm.organizationId) {
      alert(this.t('manageRepresentatives.organizationRequired'));
      return;
    }

    const dto = { ...this.repForm };
    this.adminService.createRepresentative(dto).subscribe({
      next: () => {
        alert(this.t('manageRepresentatives.createSuccess'));
        this.repForm = {
          name: '',
          citizenId: '',
          nationality: '',
          email: '',
          phoneNumber: '',
          organizationId: ''
        };
      },
      error: err => {
        console.error(err);
        alert(this.t('manageRepresentatives.createError'));
      }
    });
  }

  goBack() {
    this.router.navigate(['/admin/manage-representatives']);
  }
}
