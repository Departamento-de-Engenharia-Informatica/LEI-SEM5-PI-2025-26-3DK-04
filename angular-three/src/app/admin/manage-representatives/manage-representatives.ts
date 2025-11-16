import {
  Component,
  OnInit,
  ChangeDetectorRef,
  ViewChild,
  Inject,
  PLATFORM_ID
} from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { FormsModule, NgForm } from '@angular/forms';
import {firstValueFrom, Observable, of} from 'rxjs';
import { AdminService } from '../admin.service';
import { TranslationService } from '../../translation.service';
import { Router } from '@angular/router';
import {catchError} from 'rxjs/operators';

interface Representative {
  name: string;
  email: string;
  citizenId: string;
  organizationId: string;
  phoneNumber: string;
  status: 'Active' | 'Inactive';
}

interface Organization {
  id: string;
  legalName: string;
}

interface RepresentativeForm {
  name: string;
  citizenId: string;
  nationality: string;
  email: string;
  phoneCountry: { code: string; flag: string; name: string };
  phoneNumber: string;
  organizationId: string;
}

interface CountryCode {
  code: string;
  flag: string;
  name: string;
}

@Component({
  selector: 'app-manage-representatives',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './manage-representatives.html',
  styleUrls: ['./manage-representatives.scss']
})
export class ManageRepresentatives implements OnInit {
  @ViewChild('repNgForm') repNgForm!: NgForm;

  isBrowser: boolean;

  nationalities: string[] = ['PT', 'ES', 'FR', 'UK'];

  countryCodes: CountryCode[] = [
    { code: '+351', flag: '🇵🇹', name: 'Portugal' },
    { code: '+34', flag: '🇪🇸', name: 'Espanha' },
    { code: '+33', flag: '🇫🇷', name: 'França' }
  ];

  representatives: Representative[] = [];
  filteredRepresentatives: Representative[] = [];

  organizations: Organization[] = [];
  filteredOrganizations: Organization[] = [];

  orgSearchTerm: string = '';
  repSearchTerm: string = '';

  loadingReps: boolean = false;
  loadingOrgs: boolean = false;

  currentLang = 'en';

  repForm: RepresentativeForm;
  editingRep: boolean = false;

  // NEW — original values
  originalCitizenId: string | undefined;
  originalEmail: string | undefined;
  originalPhone: string | undefined;

  // REMOVED: per-field edit flags are now redundant and the logic is handled in the validation methods

  errors: any = {};
  isSaving: boolean = false;

  statusModalOpen: boolean = false;
  statusTarget: Representative | null = null;
  statusSelected: 'Active' | 'Inactive' = 'Inactive';
  statusOptions = ['Active', 'Inactive'] as const;

  constructor(
    private adminService: AdminService,
    private translation: TranslationService,
    private router: Router,
    private cdr: ChangeDetectorRef,
    @Inject(PLATFORM_ID) private platformId: Object
  ) {
    this.isBrowser = isPlatformBrowser(this.platformId);

    if (this.isBrowser) {
      const savedLang = localStorage.getItem('appLang');
      if (savedLang) this.translation.setLanguage(savedLang);
      this.currentLang = savedLang || 'en';
    }

    this.repForm = this.getInitialForm();
  }

  t(key: string) {
    return this.translation.translate(key);
  }

  displayMessage(msg: string) {
    if (this.isBrowser) alert(msg);
    else console.log('SERVER MSG:', msg);
  }

  normalize(str: string): string {
    return str.trim().toLowerCase();
  }

  normalizePhone(phone: string): string {
    return phone.replace(/[^0-9+]/g, '');
  }

  ngOnInit(): void {
    this.loadOrganizations();
    this.loadRepresentatives();
  }

  getInitialForm(): RepresentativeForm {
    return {
      name: '',
      citizenId: '',
      nationality: this.nationalities[0],
      email: '',
      phoneCountry: this.countryCodes[0],
      phoneNumber: '',
      organizationId: ''
    };
  }

  loadOrganizations() {
    this.loadingOrgs = true;

    this.adminService.getAllOrganizations().subscribe({
      next: res => {
        this.organizations = res;
        this.filteredOrganizations = res;
        this.loadingOrgs = false;
        this.cdr.detectChanges();
      },
      error: err => {
        this.displayMessage(
          this.t('manageRepresentatives.loadOrganizationsError') +
          ': ' +
          (err?.error?.message || JSON.stringify(err))
        );
        this.loadingOrgs = false;
      }
    });
  }

  loadRepresentatives() {
    this.loadingReps = true;

    this.adminService.getAllRepresentatives().subscribe({
      next: reps => {
        this.representatives = reps.map(r => ({
          ...r,
          status:
            r.status === true || String(r.status).toLowerCase() === 'active'
              ? 'Active'
              : 'Inactive'
        }));
        this.applyRepFilter();
        this.loadingReps = false;
        this.cdr.detectChanges();
      },
      error: err => {
        this.displayMessage(
          this.t('manageRepresentatives.loadRepresentativesError') +
          ': ' +
          (err?.error?.message || JSON.stringify(err))
        );
        this.loadingReps = false;
      }
    });
  }

  applyOrgFilter() {
    const term = this.orgSearchTerm.toLowerCase();
    this.filteredOrganizations = this.organizations.filter(
      org =>
        org.id.toLowerCase().includes(term) ||
        org.legalName.toLowerCase().includes(term)
    );
  }

  applyRepFilter() {
    const term = this.repSearchTerm.toLowerCase();
    this.filteredRepresentatives = this.representatives.filter(
      rep =>
        rep.name.toLowerCase().includes(term) ||
        rep.email.toLowerCase().includes(term) ||
        rep.citizenId.toLowerCase().includes(term) ||
        rep.organizationId.toLowerCase().includes(term) ||
        rep.status.toLowerCase().includes(term)
    );
  }

  clearRepFilter() {
    this.repSearchTerm = '';
    this.applyRepFilter();
  }

  /**
   * Validates Citizen ID uniqueness, skipping remote check if in edit mode and the value is unchanged.
   */
  async validateCitizenId(citizenId: string): Promise<boolean> {
    this.errors['citizenId'] = '';

    const normalized = this.normalize(citizenId);
    const regex = /^[a-zA-Z0-9]{5,20}$/;

    if (!regex.test(citizenId)) {
      this.errors['citizenId'] = this.t('validation.citizenIdFormat');
      return false;
    }

    // New Logic: If editing and the value is the same as the original, it's valid.
    if (this.editingRep && this.originalCitizenId) {
      if (this.normalize(this.originalCitizenId) === normalized) {
        return true;
      }
    }

    try {
      // Perform remote existence check only if creating OR value has changed
      const exists: any = await firstValueFrom(this.adminService.getRepresentativeById(citizenId));
      if (exists !== null && exists !== undefined) {
        this.errors['citizenId'] = this.t('validation.citizenIdExists');
        return false;
      }
      return true;
    } catch {
      this.errors['citizenId'] = this.t('validation.serverErrorCitizenId');
      return false;
    }
  }

  /**
   * Validates Email uniqueness, skipping remote check if in edit mode and the value is unchanged.
   */
  async validateEmail(email: string, isEditing = false, originalEmail?: string): Promise<boolean> {
    // inicializa erro
    this.errors['email'] = '';

    const normalized = (email || '').trim().toLowerCase();
    if (!normalized) {
      this.errors['email'] = this.t('manageUsers.emailRequired') || 'O email é obrigatório.';
      return false;
    }

    // 1. Validar formato @gmail.com
    if (!/^[^\s@]+@gmail\.com$/i.test(normalized)) {
      this.errors['email'] = this.t('manageUsers.emailMustBeGmail') || 'O email deve terminar em @gmail.com';
      return false;
    }

    // 2. Verificar unicidade dentro do formulário (outros reps do mesmo form)
    if (this.repForm) {
      const otherEmails = [this.repForm.email] // você pode ajustar se tiver array de reps
        .map(e => (e || '').trim().toLowerCase())
        .filter(e => e && e !== normalized);

      if (otherEmails.includes(normalized)) {
        this.errors['email'] = this.t('validation.duplicateEmailInForm') || 'Este email já existe no formulário.';
        return false;
      }
    }

    // 3. Se estiver editando e o email não mudou, passa
    if (isEditing && originalEmail && normalized === originalEmail.toLowerCase()) {
      return true;
    }

    try {
      // 4. Verificar se existe como representante
      const repExists = await firstValueFrom(this.adminService.checkRepresentativeEmailExists(email));

      // 5. Verificar se existe como usuário
      let existingUsers: any[] = [];
      try {
        const users = await firstValueFrom(
          this.adminService.GetUserByEmail(normalized).pipe(
            catchError(() => of([]))
          )
        );
        existingUsers = users ? (Array.isArray(users) ? users : [users]) : [];
      } catch {
        existingUsers = [];
      }

      // 6. Se existir em qualquer fonte, retorna erro
      if (repExists || (existingUsers && existingUsers.length > 0)) {
        this.errors['email'] = this.t('validation.emailExists') || 'Este email já está registado.';
        return false;
      }

      // 7. Email válido e único
      return true;

    } catch (e) {
      console.error('Erro ao validar email:', e);
      this.errors['email'] = this.t('validation.serverErrorEmail') || 'Erro ao comunicar com o servidor para verificar o email.';
      return false;
    }
  }


  /**
   * Validates Phone Number uniqueness, skipping remote check if in edit mode and the value is unchanged.
   */
  async validatePhone(countryCode: string, phoneNumber: string): Promise<boolean> {
    this.errors['phoneNumber'] = '';

    if (!/^\d{9}$/.test(phoneNumber)) {
      this.errors['phoneNumber'] = this.t('validation.phoneNumberFormat');
      return false;
    }

    const full = countryCode + phoneNumber;
    const normalizedNew = this.normalizePhone(full);

    // New Logic: If editing and the value is the same as the original, it's valid.
    if (this.editingRep && this.originalPhone) {
      if (this.normalizePhone(this.originalPhone) === normalizedNew) {
        return true;
      }
    }

    try {
      // Perform remote existence check only if creating OR value has changed
      const exists = await this.adminService
        .checkRepresentativePhoneExists(full)
        .toPromise();

      if (exists) {
        this.errors['phoneNumber'] = this.t('validation.phoneNumberExists');
        return false;
      }
      return true;
    } catch {
      this.errors['phoneNumber'] = this.t('validation.serverErrorPhone');
      return false;
    }
  }

  async validateOrgId(orgId: string): Promise<boolean> {
    this.errors['organizationId'] = '';

    const regex = /^[a-zA-Z0-9]{1,10}$/;
    if (!regex.test(orgId)) {
      this.errors['organizationId'] = this.t('validation.orgIdFormat');
      return false;
    }

    const exists = this.organizations.some(o => o.id === orgId);
    if (!exists) {
      this.errors['organizationId'] = this.t('validation.orgIdNotExists');
      return false;
    }

    return true;
  }

  editRepresentative(rep: Representative) {
    this.editingRep = true;

    // Encontrar o código do país
    const country = this.countryCodes.find(c => rep.phoneNumber.startsWith(c.code));

    // Extrair apenas os últimos 9 dígitos do número
    const rawNumber = country
      ? rep.phoneNumber.slice(country.code.length)
      : rep.phoneNumber;
    const number = rawNumber.slice(-9); // garante só os últimos 9 dígitos

    this.repForm = {
      name: rep.name,
      citizenId: rep.citizenId,
      nationality: this.nationalities[0],
      email: rep.email,
      organizationId: rep.organizationId,
      phoneCountry: country ?? this.countryCodes[0], // seleciona o dropdown corretamente
      phoneNumber: number
    };

    // Armazenar valores originais
    this.originalCitizenId = rep.citizenId;
    this.originalEmail = rep.email;
    this.originalPhone = rep.phoneNumber;

    this.errors = {};
  }


  async saveRepresentative() {
    if (!this.repNgForm.valid || this.isSaving) {
      this.displayMessage(this.t('common.formInvalid'));
      return;
    }

    this.isSaving = true;
    this.errors = {};

    const form = this.repForm;
    const isEditing = this.editingRep;

    // The validation methods now handle the check against original values internally.
    const [isCitizenValid, isEmailValid, isPhoneValid, isOrgValid] = await Promise.all([
      this.validateCitizenId(form.citizenId),
      this.validateEmail(form.email),
      this.validatePhone(form.phoneCountry.code, form.phoneNumber),
      this.validateOrgId(form.organizationId)
    ]);

    if (!isCitizenValid || !isEmailValid || !isPhoneValid || !isOrgValid) {
      this.displayMessage(this.t('common.formValidationFailed'));
      this.isSaving = false;
      this.cdr.detectChanges();
      return;
    }

    if (isEditing) {
      const updateDto = {
        CitizenId: form.citizenId,
        Name: form.name,
        Email: form.email,
        PhoneNumber: form.phoneCountry.code + form.phoneNumber,
        Nationality: form.nationality
      };

      this.adminService.updateRepresentative(this.originalCitizenId!, updateDto).subscribe({
        next: () => {
          this.displayMessage(this.t('manageRepresentatives.updateSuccess'));
          this.resetForm();
          this.loadRepresentatives();
          this.isSaving = false;
        },
        error: err => {
          this.displayMessage(
            this.t('common.saveError') +
            ': ' +
            (err?.error?.message || JSON.stringify(err))
          );
          this.isSaving = false;
          this.cdr.detectChanges();
        }
      });
    } else {
      const createDto = {
        Name: form.name,
        CitizenId: form.citizenId,
        Nationality: form.nationality,
        Email: form.email,
        PhoneNumber: form.phoneCountry.code + form.phoneNumber,
        OrganizationId: form.organizationId
      };

      this.adminService.createRepresentative(createDto).subscribe({
        next: () => {
          this.displayMessage(this.t('manageRepresentatives.createSuccess'));
          this.resetForm();
          this.loadRepresentatives();
          this.isSaving = false;
        },
        error: err => {
          this.displayMessage(
            this.t('common.saveError') +
            ': ' +
            (err?.error?.message || JSON.stringify(err))
          );
          this.isSaving = false;
          this.cdr.detectChanges();
        }
      });
    }
  }

  resetForm() {
    this.editingRep = false;
    this.repForm = this.getInitialForm();
    this.errors = {};

    // Clear original values
    this.originalCitizenId = undefined;
    this.originalEmail = undefined;
    this.originalPhone = undefined;

    // REMOVED: isEditingCitizenId, isEditingEmail, isEditingPhone flags

    if (this.repNgForm) {
      this.repNgForm.resetForm(this.repForm);
    }
  }

  openChangeStatusDialog(rep: Representative) {
    this.statusTarget = rep;
    this.statusSelected = rep.status;
    this.statusModalOpen = true;
  }

  closeStatusModal() {
    this.statusModalOpen = false;
    this.statusTarget = null;
  }

  confirmChangeStatus() {
    if (!this.statusTarget?.citizenId) return;

    const repId = this.statusTarget.citizenId;

    let request$: Observable<any>;
    let successKey: string;
    let errorKey: string;

    if (this.statusSelected === 'Active') {
      request$ = this.adminService.activateRepresentative(repId);
      successKey = 'manageRepresentatives.statusSuccess.activate';
      errorKey = 'manageRepresentatives.statusError.activate';
    } else {
      request$ = this.adminService.deactivateRepresentative(repId);
      successKey = 'manageRepresentatives.statusSuccess.deactivate';
      errorKey = 'manageRepresentatives.statusError.deactivate';
    }

    request$.subscribe({
      next: () => {
        this.displayMessage(this.t(successKey));
        this.closeStatusModal();
        this.loadRepresentatives();
      },
      error: err => {
        this.displayMessage(
          this.t(errorKey) + ': ' + (err?.error?.message || JSON.stringify(err))
        );
      }
    });
  }

  compareCountryCode(c1: CountryCode, c2: CountryCode): boolean {
    return c1 && c2 ? c1.code === c2.code : c1 === c2;
  }
}
