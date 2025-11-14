import { Component, Inject, PLATFORM_ID, ViewChild } from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { FormsModule, NgForm } from '@angular/forms';
import { TranslationService } from '../../../translation.service';
import { AdminService } from '../../admin.service';
import { Router } from '@angular/router';
import { of } from 'rxjs';
import { catchError } from 'rxjs/operators';

@Component({
  selector: 'app-edit-representative',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './edit-representative.html',
  styleUrls: ['./edit-representative.scss']
})
export class EditRepresentative {
  @ViewChild('repNgForm') repNgForm!: NgForm;

  representatives: any[] = [];
  filteredRepresentatives: any[] = [];
  searchTerm = '';
  repForm: any = null;
  currentLang = 'en';

  countryCodes = [
    { code: '+351', name: 'Portugal', flag: '🇵🇹' },
    { code: '+34', name: 'Spain', flag: '🇪🇸' },
    { code: '+44', name: 'United Kingdom', flag: '🇬🇧' },
    { code: '+33', name: 'France', flag: '🇫🇷' },
    { code: '+39', name: 'Italy', flag: '🇮🇹' },
    { code: '+49', name: 'Germany', flag: '🇩🇪' }
  ];

  nationalities = ['PT', 'ES', 'FR', 'IT', 'GR', 'UK'];

  errors: any = {};
  editing = false;

  constructor(
    private adminService: AdminService,
    private translation: TranslationService,
    private router: Router,
    @Inject(PLATFORM_ID) private platformId: Object
  ) {
    const isBrowser = isPlatformBrowser(this.platformId);
    if (isBrowser) {
      const savedLang = localStorage.getItem('appLang');
      if (savedLang) this.translation.setLanguage(savedLang);
      this.currentLang = savedLang || 'en';
    }
  }

  t(key: string) { return this.translation.translate(key); }

  ngOnInit() { this.loadRepresentatives() }

  loadRepresentatives() {
    this.adminService.getAllRepresentatives().subscribe({
      next: res => {
        this.representatives = res;
        this.applyFilter();
      },
      error: () => alert(this.t('manageRepresentatives.loadError'))
    });
  }

  applyFilter() {
    const q = (this.searchTerm || '').toLowerCase();
    this.filteredRepresentatives = this.representatives.filter(r =>
      (r.name || '').toLowerCase().includes(q) ||
      (r.email || '').toLowerCase().includes(q) ||
      ((r.citizenId ?? r.id) || '').toLowerCase().includes(q)
    );
  }

  selectRepresentative(rep: any) {
    this.editing = true;
    this.repForm = {
      ...rep,
      phoneNumber: rep.phoneNumber?.replace(rep.phoneCountry?.code || '+351', '') || '',
      phoneCountry: rep.phoneCountry || { code: '+351', name: 'Portugal', flag: '🇵🇹' },
      nationality: rep.nationality || ''
    };
    this.errors = {};
  }

  saveRepresentative() {
    if (!this.repForm) return;

    // Validations
    if (!this.validateEmail() || !this.validatePhone()) {
      alert(this.t('manageRepresentatives.invalidFields'));
      return;
    }

    const id = this.repForm.citizenId ?? this.repForm.id;
    const payload = {
      ...this.repForm,
      phoneNumber: `${this.repForm.phoneCountry.code}${this.repForm.phoneNumber}`
    };

    this.adminService.updateRepresentative(id, payload).subscribe({
      next: () => {
        alert(this.t('manageRepresentatives.updateSuccess'));
        this.clearForm();
        this.loadRepresentatives();
      },
      error: () => alert(this.t('manageRepresentatives.updateError'))
    });
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
    this.repForm = null;
    this.editing = false;
    this.errors = {};
  }

  goBack() {
    this.router.navigate(['/admin/manage-representatives']);
  }
}
