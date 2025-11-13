import { Component, Inject, PLATFORM_ID } from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TranslationService } from '../../translation.service';
import { AdminService } from '../admin.service';

@Component({
  selector: 'app-manage-organizations',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './manage-organizations.html',
  styleUrls: ['./manage-organizations.scss']
})
export class ManageOrganizations {
  organizations: any[] = [];
  editing = false;
  editingId: string | null = null;
  currentLang = 'en';

  orgForm = {
    id: '',
    legalName: '',
    alternativeName: '',
    address: '',
    taxNumber: '',
    representatives: [
      { name: '', citizenId: '', nationality: '', email: '', phoneNumber: '' }
    ]
  };

  constructor(
    private adminService: AdminService,
    private translation: TranslationService,
    @Inject(PLATFORM_ID) private platformId: Object
  ) {
    const isBrowser = isPlatformBrowser(this.platformId);
    if (isBrowser) {
      const savedLang = localStorage.getItem('appLang');
      if (savedLang) {
        this.translation.setLanguage(savedLang);
        this.currentLang = savedLang;
      }
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
      next: res => (this.organizations = res),
      error: err => console.error('Erro ao carregar organizações:', err)
    });
  }

  addRepresentative() {
    this.orgForm.representatives.push({ name: '', citizenId: '', nationality: '', email: '', phoneNumber: '' });
  }

  removeRepresentative(index: number) {
    if (this.orgForm.representatives.length > 1) {
      this.orgForm.representatives.splice(index, 1);
    }
  }

  saveOrganization() {
    if (!this.orgForm.legalName || !this.orgForm.address || !this.orgForm.taxNumber) {
      alert(this.t('manageOrganizations.missingFields'));
      return;
    }

    const dto = {
      id: this.orgForm.id,
      legalName: this.orgForm.legalName,
      alternativeName: this.orgForm.alternativeName,
      address: this.orgForm.address,
      taxNumber: this.orgForm.taxNumber,
      representatives: this.orgForm.representatives
    };

    this.adminService.createOrganization(dto).subscribe({
      next: () => {
        alert(this.t('manageOrganizations.createSuccess'));
        this.loadOrganizations();
        this.resetForm();
      },
      error: err => {
        console.error(err);
        alert(this.t('manageOrganizations.createError'));
      }
    });
  }

  resetForm() {
    this.editing = false;
    this.editingId = null;
    this.orgForm = {
      id: '',
      legalName: '',
      alternativeName: '',
      address: '',
      taxNumber: '',
      representatives: [
        { name: '', citizenId: '', nationality: '', email: '', phoneNumber: '' }
      ]
    };
  }
}
