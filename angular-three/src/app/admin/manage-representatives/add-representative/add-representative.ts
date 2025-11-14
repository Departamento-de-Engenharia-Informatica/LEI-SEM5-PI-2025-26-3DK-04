import { Component, Inject, PLATFORM_ID, ViewChild } from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { FormsModule, NgForm } from '@angular/forms';
import { TranslationService } from '../../../translation.service';
import { AdminService } from '../../admin.service';
import { Router } from '@angular/router';
import { of } from 'rxjs';
import { catchError } from 'rxjs/operators';

@Component({
  selector: 'app-add-representative',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './add-representative.html',
  styleUrls: ['./add-representative.scss']
})
export class AddRepresentative {
  @ViewChild('repNgForm') repNgForm!: NgForm;

  repForm = {
    name: '',
    citizenId: '',
    nationality: '',
    email: '',
    phoneNumber: '',
    phoneCountry: { code: '+351', name: 'Portugal', flag: '🇵🇹' },
    organizationId: ''
  };

  organizations: any[] = [];
  filteredOrganizations: any[] = [];
  searchTerm = '';
  currentLang = 'en';

  countryCodes = [
    { code: '+351', name: 'Portugal', flag: '🇵🇹' },
    { code: '+34', name: 'Spain', flag: '🇪🇸' },
    { code: '+44', name: 'United Kingdom', flag: '🇬🇧' },
    { code: '+33', name: 'France', flag: '🇫🇷' },
    { code: '+39', name: 'Italy', flag: '🇮🇹' },
    { code: '+49', name: 'Germany', flag: '🇩🇪' }
  ];

  nationalities = ['PT','ES','FR','IT','GR','UK'];

  errors: any = {};

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

  t(key: string) { return this.translation.translate(key); }

  ngOnInit() { this.loadOrganizations(); }

  loadOrganizations() {
    this.adminService.getAllOrganizations().subscribe({
      next: res => {
        this.organizations = res;
        this.applyFilter();
      },
      error: () => alert(this.t('manageRepresentatives.loadOrganizationsError'))
    });
  }

  applyFilter() {
    const q = (this.searchTerm || '').toLowerCase();
    this.filteredOrganizations = this.organizations.filter(org =>
      (org.legalName || '').toLowerCase().includes(q) ||
      (org.id || '').toLowerCase().includes(q)
    );
  }

  validateEmail() {
    this.errors.email = '';
    const email = (this.repForm.email || '').trim();
    if (!/^[^\s@]+@gmail\.com$/i.test(email)) {
      this.errors.email = this.t('manageRepresentatives.emailMustBeGmail');
      return false;
    }
    return true;
  }

  validateCitizenId() {
    this.errors.citizenId = '';
    const cid = (this.repForm.citizenId || '').trim();
    if (!/^[a-zA-Z0-9]{5,20}$/.test(cid)) {
      this.errors.citizenId = this.t('manageRepresentatives.citizenIdInvalid');
      return false;
    }
    return true;
  }

  validatePhone() {
    this.errors.phoneNumber = '';
    const pn = (this.repForm.phoneNumber || '').trim();
    if (!pn) {
      this.errors.phoneNumber = this.t('manageRepresentatives.phoneRequired');
      return false;
    }
    return true;
  }

  clearForm() {
    if (this.repNgForm) this.repNgForm.resetForm();
    this.repForm = {
      name: '',
      citizenId: '',
      nationality: '',
      email: '',
      phoneNumber: '',
      phoneCountry: { code: '+351', name: 'Portugal', flag: '🇵🇹' },
      organizationId: ''
    };
    this.errors = {};
  }

  saveRepresentative() {
    if (!this.repForm.organizationId) {
      alert(this.t('manageRepresentatives.organizationRequired'));
      return;
    }

    if (!this.validateCitizenId() || !this.validateEmail() || !this.validatePhone()) {
      alert(this.t('manageRepresentatives.invalidFields'));
      return;
    }

    const dto = { ...this.repForm, phoneNumber: `${this.repForm.phoneCountry.code}${this.repForm.phoneNumber}` };

    this.adminService.createRepresentative(dto).subscribe({
      next: () => {
        alert(this.t('manageRepresentatives.createSuccess'));
        this.clearForm();
        this.loadOrganizations();
      },
      error: () => alert(this.t('manageRepresentatives.createError'))
    });
  }

  goBack() {
    this.router.navigate(['/admin/manage-representatives']);
  }
}
