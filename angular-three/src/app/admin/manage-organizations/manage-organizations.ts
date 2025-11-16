import { Component, Inject, PLATFORM_ID, ChangeDetectorRef } from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TranslationService } from '../../translation.service';
import { AdminService } from '../admin.service';
import {firstValueFrom, of} from 'rxjs';
import {catchError} from 'rxjs/operators';

interface RepresentativeForm {
  name: string;
  citizenId: string;
  nationality: string;
  email: string;
  phoneCountry: { code: string; flag: string; name: string };
  phoneNumber: string;
}

interface OrgForm {
  id: string;
  legalName: string;
  alternativeName: string;
  address: string;
  taxCountry: string;
  taxNumber: string;
  phoneCountry: { code: string; flag: string; name: string };
  representatives: RepresentativeForm[];
}

interface RepErrors {
  citizenId: string;
  email: string;
  phoneNumber: string;
  name?: string;
}

interface OrgErrors {
  orgId: string;
  legalName: string;
  taxNumber: string;
  reps: RepErrors[];
  phoneNumber: string;
}

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
  isBrowser: boolean;

  nationalities: string[] = ['PT', 'ES', 'FR'];
  countryCodes = [
    { code: '+351', flag: '🇵🇹', name: 'Portugal' },
    { code: '+34', flag: '🇪🇸', name: 'Espanha' },
    { code: '+33', flag: '🇫🇷', name: 'França' },
  ];

  orgForm: OrgForm = {
    id: '',
    legalName: '',
    alternativeName: '',
    address: '',
    taxCountry: 'PT',
    taxNumber: '',
    phoneCountry: { code: '+351', flag: '🇵🇹', name: 'Portugal' },
    representatives: [
      {
        name: '',
        citizenId: '',
        nationality: 'PT',
        email: '',
        phoneNumber: '',
        phoneCountry: { code: '+351', flag: '🇵🇹', name: 'Portugal' }
      }
    ]
  };

  errors: OrgErrors = {
    orgId: '',
    legalName: '',
    taxNumber: '',
    reps: [],
    phoneNumber: ''
  };

  globalErrorMessage = '';

  constructor(
    private adminService: AdminService,
    private translation: TranslationService,
    @Inject(PLATFORM_ID) private platformId: Object,
    private cdr: ChangeDetectorRef
  ) {
    this.isBrowser = isPlatformBrowser(platformId);
    if (this.isBrowser) {
      const savedLang = localStorage.getItem('appLang');
      if (savedLang) {
        this.translation.setLanguage(savedLang);
        this.currentLang = savedLang;
      }
    }
  }

  t(key: string): string {
    return this.translation.translate(key);
  }

  ngOnInit() {
    this.loadOrganizations();
    this.ensureRepErrorsLength();
  }

  loadOrganizations() {
    this.adminService.getAllOrganizations().subscribe({
      next: res => this.organizations = res,
      error: err => console.error('Erro ao carregar organizações:', err)
    });
  }

  ensureRepErrorsLength() {
    while (this.errors.reps.length < this.orgForm.representatives.length) {
      this.errors.reps.push({ citizenId: '', email: '', phoneNumber: '' });
    }
    while (this.errors.reps.length > this.orgForm.representatives.length) {
      this.errors.reps.pop();
    }
  }

  addRepresentative() {
    this.orgForm.representatives.push({
      name: '',
      citizenId: '',
      nationality: 'PT',
      email: '',
      phoneNumber: '',
      phoneCountry: this.countryCodes[0]
    });
    this.ensureRepErrorsLength();
  }

  removeRepresentative(index: number) {
    if (this.orgForm.representatives.length > 1) {
      this.orgForm.representatives.splice(index, 1);
      this.ensureRepErrorsLength();
    }
  }

  resetForm() {
    this.editing = false;
    this.editingId = null;
    this.orgForm = {
      id: '',
      legalName: '',
      alternativeName: '',
      address: '',
      taxCountry: 'PT',
      taxNumber: '',
      phoneCountry: { code: '+351', flag: '🇵🇹', name: 'Portugal' },
      representatives: [
        {
          name: '',
          citizenId: '',
          nationality: 'PT',
          email: '',
          phoneNumber: '',
          phoneCountry: { code: '+351', flag: '🇵🇹', name: 'Portugal' }
        }
      ]
    };
    this.errors = { orgId: '', legalName: '', taxNumber: '', reps: [], phoneNumber: '' };
    this.ensureRepErrorsLength();
    this.globalErrorMessage = '';
    this.cdr.detectChanges();
  }

  validateOrgFormSync(): boolean {
    let valid = true;

    this.errors.orgId = '';
    this.errors.legalName = '';
    this.errors.taxNumber = '';
    this.globalErrorMessage = '';

    if (!/^[a-zA-Z0-9]{1,10}$/.test(this.orgForm.id)) {
      this.errors.orgId = this.t('validation.orgIdFormat') || 'Organization ID must be 1-10 alphanumeric characters.';
      valid = false;
    }

    if (!this.orgForm.legalName || this.orgForm.legalName.trim().length === 0) {
      this.errors.legalName = this.t('validation.required') || 'Legal name is required.';
      valid = false;
    }

    if (!this.orgForm.taxNumber || this.orgForm.taxNumber.trim().length <= 1) {
      this.errors.taxNumber = this.t('validation.taxNumberMinLength') || 'Tax number too short.';
      valid = false;
    }

    this.orgForm.representatives.forEach((rep, i) => {
      if (!this.errors.reps[i]) this.errors.reps[i] = { citizenId: '', email: '', phoneNumber: '' };

      if (!rep.name || rep.name.trim().length === 0) {
        this.errors.reps[i].name = this.t('validation.required') || 'Name is required.';
        valid = false;
      } else {
        this.errors.reps[i].name = '';
      }

      if (!/^[a-zA-Z0-9]{5,20}$/.test(rep.citizenId)) {
        this.errors.reps[i].citizenId = this.t('validation.citizenIdFormat') || 'Citizen ID must be 5-20 alphanumeric characters.';
        valid = false;
      } else {
        this.errors.reps[i].citizenId = '';
      }

      if (!/^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/.test(rep.email)) {
        this.errors.reps[i].email = this.t('validation.emailFormat') || 'Invalid email format.';
        valid = false;
      } else {
        this.errors.reps[i].email = '';
      }

      if (!/^\d{9}$/.test(rep.phoneNumber)) {
        this.errors.reps[i].phoneNumber = this.t('validation.phoneFormat') || 'Phone number must have 9 digits.';
        valid = false;
      } else {
        this.errors.reps[i].phoneNumber = '';
      }
    });

    const citizenIds = this.orgForm.representatives.map(r => (r.citizenId || '').trim().toLowerCase()).filter(x => x);
    if (new Set(citizenIds).size !== citizenIds.length) {
      this.globalErrorMessage = this.t('validation.duplicateCitizenIdInForm') || 'Duplicate citizen ID in the form.';
      valid = false;
    }

    const emails = this.orgForm.representatives.map(r => (r.email || '').trim().toLowerCase()).filter(x => x);
    if (new Set(emails).size !== emails.length) {
      this.globalErrorMessage = this.t('validation.duplicateEmailInForm') || 'Duplicate email in the form.';
      valid = false;
    }

    const phones = this.orgForm.representatives.map(r => (r.phoneCountry.code || '') + (r.phoneNumber || '')).filter(x => x);
    if (new Set(phones).size !== phones.length) {
      this.globalErrorMessage = this.t('validation.duplicatePhoneInForm') || 'Duplicate phone number in the form.';
      valid = false;
    }

    this.cdr.detectChanges();
    return valid;
  }

  private normalize(s: string | undefined | null): string {
    return (s || '').trim().toLowerCase();
  }

  private normalizePhone(phone: string): string {
    return (phone || '').replace(/\D/g, '');
  }

  async validateCitizenIdAsync(citizenId: string, index: number, isEditing = false, originalCitizenId?: string): Promise<boolean> {
    this.errors.reps[index].citizenId = '';
    const normalized = this.normalize(citizenId);
    const regex = /^[a-zA-Z0-9]{5,20}$/;

    if (!regex.test(citizenId)) {
      this.errors.reps[index].citizenId = this.t('validation.citizenIdFormat') || 'Citizen ID format invalid.';
      return false;
    }

    if (isEditing && originalCitizenId && this.normalize(originalCitizenId) === normalized) {
      return true;
    }

    try {
      const exists: any = await firstValueFrom(this.adminService.getRepresentativeById(citizenId));
      if (exists !== null && exists !== undefined) {
        this.errors.reps[index].citizenId = this.t('validation.citizenIdExists') || 'Citizen ID already exists.';
        return false;
      }
      return true;
    } catch (e) {
      this.errors.reps[index].citizenId = this.t('validation.serverErrorCitizenId') || 'Server error validating citizen ID.';
      return false;
    }
  }

  async validateEmailAsync(email: string, index: number, isEditing = false, originalEmail?: string): Promise<boolean> {
    this.errors.reps[index].email = '';

    const normalized = (email || '').trim().toLowerCase();
    if (!normalized) {
      this.errors.reps[index].email = this.t('manageUsers.emailRequired') || 'O email é obrigatório.';
      return false;
    }

    // 1. Validar formato @gmail.com
    if (!/^[^\s@]+@gmail\.com$/i.test(normalized)) {
      this.errors.reps[index].email = this.t('manageUsers.emailMustBeGmail') || 'O email deve terminar em @gmail.com';
      return false;
    }

    // 2. Unicidade dentro do formulário (outros representantes)
    const emailsInForm = this.orgForm.representatives
      .filter((_, i) => i !== index)
      .map(r => (r.email || '').trim().toLowerCase())
      .filter(x => x);
    if (emailsInForm.includes(normalized)) {
      this.errors.reps[index].email = this.t('validation.duplicateEmailInForm') || 'Este email já existe no formulário.';
      return false;
    }

    // 3. Se estiver editando e não mudou o email, passa
    if (isEditing && originalEmail && normalized === originalEmail.toLowerCase()) {
      return true;
    }

    try {
      // 4. Verificar se email existe como representante
      const repExists = await firstValueFrom(this.adminService.checkRepresentativeEmailExists(email));

      // 5. Verificar se email existe na base de usuários
      let existingUsers: any[] = [];
      try {
        const users = await firstValueFrom(
          this.adminService.GetUserByEmail(normalized).pipe(
            catchError(error => {
              console.warn('GetUserByEmail erro (tratado como não existente):', error);
              return of([]);
            })
          )
        );
        existingUsers = users ? (Array.isArray(users) ? users : [users]) : [];
      } catch (err) {
        existingUsers = [];
      }

      // 6. Se existir em qualquer fonte, retorna erro
      if (repExists ||( existingUsers && existingUsers.length > 0)) {
        this.errors.reps[index].email = this.t('validation.emailExists') || 'Este email já está registado.';
        return false;
      }

      // 7. Email válido e único
      return true;

    } catch (e) {
      console.error('Erro ao validar email:', e);
      this.errors.reps[index].email = this.t('validation.serverErrorEmail') || 'Erro ao comunicar com o servidor para verificar o email.';
      return false;
    }
  }


  async validatePhoneAsync(countryCode: string, phoneNumber: string, index: number, isEditing = false, originalPhone?: string): Promise<boolean> {
    this.errors.reps[index].phoneNumber = '';

    if (!/^\d{9}$/.test(phoneNumber)) {
      this.errors.reps[index].phoneNumber = this.t('validation.phoneNumberFormat') || 'Phone must be 9 digits.';
      return false;
    }

    const full = `${countryCode}${phoneNumber}`;
    const normalizedNew = this.normalizePhone(full);

    if (isEditing && originalPhone && this.normalizePhone(originalPhone) === normalizedNew) {
      return true;
    }

    try {
      const exists = await firstValueFrom(this.adminService.checkRepresentativePhoneExists(full));
      if (exists) {
        this.errors.reps[index].phoneNumber = this.t('validation.phoneNumberExists') || 'Phone already exists.';
        return false;
      }
      return true;
    } catch (e) {
      this.errors.reps[index].phoneNumber = this.t('validation.serverErrorPhone') || 'Server error validating phone.';
      return false;
    }
  }

  async saveOrganization() {
    this.globalErrorMessage = '';
    this.ensureRepErrorsLength();

    const syncValid = this.validateOrgFormSync();
    if (!syncValid) {
      if (!this.globalErrorMessage) this.globalErrorMessage = this.t('validation.formInvalid') || 'Please fix validation errors.';
      this.cdr.detectChanges();
      return;
    }

    // ---------------------------
    // CHANGED: check orgId just by checking if response is null
    // ---------------------------
    try {
      const orgResp: any = await firstValueFrom(this.adminService.getOrganizationById(this.orgForm.id));

      if (orgResp !== null && orgResp !== undefined) {
        // existe → erro
        this.errors.orgId = this.t('validation.orgIdExists') || 'Organization ID already exists.';
        this.globalErrorMessage = this.t('validation.formInvalid') || 'Please fix validation errors.';
        this.cdr.detectChanges();
        return;
      }
    } catch (err) {
      // qualquer erro de rede/server deve ser tratado como erro verdadeiro
      this.globalErrorMessage = this.t('validation.serverError') || 'Server error while checking organization ID.';
      this.cdr.detectChanges();
      return;
    }

    // check legalName uniqueness
    try {
      const legalExists = await firstValueFrom(this.adminService.checkOrganizationLegalNameExists(this.orgForm.legalName));
      if (legalExists) {
        this.errors.legalName = this.t('validation.legalNameExists') || 'Legal name already exists.';
        this.globalErrorMessage = this.t('validation.formInvalid') || 'Please fix validation errors.';
        this.cdr.detectChanges();
        return;
      }
    } catch (e) {
      this.globalErrorMessage = this.t('validation.serverError') || 'Server error while checking legal name.';
      this.cdr.detectChanges();
      return;
    }

    // check taxNumber uniqueness
    const composedTax = `${this.orgForm.taxCountry}${this.orgForm.taxNumber.replace(/^[A-Z]{0,2}/i, '')}`;
    try {
      const taxExists = await firstValueFrom(this.adminService.checkOrganizationTaxNumberExists(composedTax));
      if (taxExists) {
        this.errors.taxNumber = this.t('validation.taxNumberExists') || 'Tax number already exists.';
        this.globalErrorMessage = this.t('validation.formInvalid') || 'Please fix validation errors.';
        this.cdr.detectChanges();
        return;
      }
    } catch (e) {
      this.globalErrorMessage = this.t('validation.serverError') || 'Server error while checking tax number.';
      this.cdr.detectChanges();
      return;
    }

    // representative async validations
    const repValidationPromises: Promise<boolean>[] = [];
    this.orgForm.representatives.forEach((rep, i) => {
      const p = (async () => {
        const okCid = await this.validateCitizenIdAsync(rep.citizenId, i);
        if (!okCid) return false;

        const okEmail = await this.validateEmailAsync(rep.email, i);
        if (!okEmail) return false;

        const okPhone = await this.validatePhoneAsync(rep.phoneCountry.code, rep.phoneNumber, i);
        if (!okPhone) return false;

        return true;
      })();
      repValidationPromises.push(p);
    });

    const repResults = await Promise.all(repValidationPromises);
    if (repResults.includes(false)) {
      if (!this.globalErrorMessage) this.globalErrorMessage = this.t('validation.formInvalid') || 'Please fix validation errors.';
      this.cdr.detectChanges();
      return;
    }

    // build DTO
    const dto = {
      Id: this.orgForm.id,
      LegalName: this.orgForm.legalName,
      AlternativeName: this.orgForm.alternativeName,
      Address: this.orgForm.address,
      TaxNumber: composedTax,
      Representatives: this.orgForm.representatives.map(r => ({
        Name: r.name,
        CitizenId: r.citizenId,
        Nationality: r.nationality,
        Email: r.email,
        PhoneNumber: `${r.phoneCountry.code}${r.phoneNumber}`
      }))
    };

    try {
      await firstValueFrom(this.adminService.createOrganization(dto));
      this.loadOrganizations();
      this.resetForm();
      alert(this.t('manageOrganizations.createdSuccess') || 'Organization created successfully.');
      this.cdr.detectChanges();
      return;
    } catch (err: any) {
      console.error('Create org error', err);
      const backendMessage = (err?.error?.message || err?.message || '').toString();
      if (backendMessage) {
        const msg = backendMessage.toLowerCase();
        if (msg.includes('legal') && msg.includes('name')) {
          this.errors.legalName = backendMessage;
        } else if (msg.includes('tax')) {
          this.errors.taxNumber = backendMessage;
        } else if (msg.includes('citizen') || msg.includes('representative')) {
          this.globalErrorMessage = backendMessage;
        } else {
          this.globalErrorMessage = backendMessage;
        }
      } else {
        this.globalErrorMessage = this.t('validation.serverError') || 'Server error while creating organization.';
      }
      this.cdr.detectChanges();
      return;
    }
  }
}
