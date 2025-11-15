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
import { firstValueFrom } from 'rxjs';
import { AdminService } from '../admin.service';
import { TranslationService } from '../../translation.service';
import { Router } from '@angular/router';
import { AuthService } from '../../auth.service';
import { catchError } from 'rxjs/operators';
import { of } from 'rxjs';

interface User {
  email: string; // ID do usuário
  name: string;
  role: string; // Ex: 'PortAuthorityOfficer', 'LogisticsOperator', 'ProjectManager'
  // Outros campos relevantes da API, se existirem
}

interface UserForm {
  email: string;
  name: string;
  role: string;
}

@Component({
  selector: 'app-manage-users',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './manage-users.html',
  styleUrls: ['./manage-users.scss']
})
export class ManageUsers implements OnInit {
  @ViewChild('userNgForm') userNgForm!: NgForm;

  users: User[] = [];
  filteredUsers: User[] = [];

  // Paginação Manual (5 por vez, com scroll simulado na tabela)
  pageSize: number = 5;
  currentPage: number = 1;

  searchTerm: string = '';
  loadingUsers: boolean = false;
  currentLang = 'en';

  userForm: UserForm;
  editingUser: boolean = false; // Flag para criar (false) ou editar (true)

  // Armazena o email original durante a edição (serve como ID)
  originalEmail: string | undefined;

  errors: any = {};
  isSaving: boolean = false;
  messageSuccess: string = '';
  messageError: string = '';

  roles: string[] = [
    'PortAuthorityOfficer',
    'LogisticsOperator',
    'ProjectManager'
  ];

  constructor(
    private adminService: AdminService,
    private translation: TranslationService,
    private router: Router,
    private cdr: ChangeDetectorRef,
    private auth: AuthService,
    @Inject(PLATFORM_ID) private platformId: Object
  ) {
    const isBrowser = isPlatformBrowser(this.platformId);
    if (isBrowser) {
      const savedLang = localStorage.getItem('appLang');
      if (savedLang) this.translation.setLanguage(savedLang);
      this.currentLang = savedLang || 'en';
    }

    this.userForm = this.getInitialForm();
  }

  t(key: string) {
    return this.translation.translate(key);
  }

  /**
   * Mostra um alerta no browser (se estiver a correr no browser).
   */
  displayMessage(msg: string) {
    if (isPlatformBrowser(this.platformId)) {
      alert(msg);
    } else {
      console.log('SERVER MSG:', msg);
    }
  }

  ngOnInit(): void {
    this.loadUsers();
  }

  getInitialForm(): UserForm {
    return {
      email: '',
      name: '',
      role: this.roles[0] // Define uma role inicial
    };
  }

  loadUsers() {
    this.loadingUsers = true;
    this.messageError = '';

    this.adminService.getAllUsers().subscribe({
      next: users => {
        const loggedEmail = this.auth.email;

        this.users = (users || []).filter(u =>
          u.role !== 'NoRole' &&
          u.role !== 'Unknown' &&
          u.email !== loggedEmail &&
          u.status !== 'Inactive' &&
          u.role !== 'Admin' &&
          u.role !== 'Representative'
        );

        this.applyFilter();
        this.loadingUsers = false;
        this.cdr.detectChanges();
      },
      error: err => {
        console.error('Load users error', err);
        this.messageError = this.t('manageUsers.loadError') || 'Erro ao carregar usuários.';
        this.loadingUsers = false;
      }
    });
  }

  applyFilter() {
    const q = (this.searchTerm || '').toLowerCase();
    const filtered = this.users.filter(u =>
      (u.name || '').toLowerCase().includes(q) ||
      (u.email || '').toLowerCase().includes(q) ||
      (u.role || '').toLowerCase().includes(q)
    );
    this.filteredUsers = filtered;
    this.currentPage = 1; // Resetar para a primeira página após o filtro
  }

  // Lógica de paginação manual
  get paginatedUsers(): User[] {
    const start = (this.currentPage - 1) * this.pageSize;
    const end = start + this.pageSize;
    return this.filteredUsers.slice(start, end);
  }

  /**
   * Valida o email: formato, fim em @gmail.com e unicidade (apenas para criação).
   * @param email Email a ser validado.
   * @param isEditing Indica se o formulário está em modo de edição.
   */
  async validateEmail(email: string, isEditing: boolean = false): Promise<boolean> {
    this.errors.email = '';

    const normalized = (email || '').trim().toLowerCase();
    if (!normalized) {
      this.errors.email = this.t('manageUsers.emailRequired') || 'O email é obrigatório.';
      return false;
    }

    // 1. Validar Formato @gmail.com
    if (!/^[^\s@]+@gmail\.com$/i.test(normalized)) {
      this.errors.email = this.t('manageUsers.emailMustBeGmail') || 'O email deve terminar em @gmail.com';
      return false;
    }

    // 2. Unicidade (apenas se for criação OU se o email tiver sido alterado na edição)
    if (isEditing) {
      if (this.originalEmail && normalized === this.originalEmail.toLowerCase()) {
        // Se estiver editando e o email não mudou, passa.
        return true;
      }
      // Se estiver editando e mudou, deve ser tratado como criação para verificar unicidade, mas o campo estará desativado, tornando este caminho incomum.
    }

    // 3. Verificar unicidade na BD (Usando GetUserByEmail)
    try {
      const existingUsers = await firstValueFrom(
        this.adminService.GetUserByEmail(normalized).pipe(
          catchError(error => {
            // Se houver erro (ex: 404 - Not Found), assumimos que o utilizador não existe.
            console.warn('GetUserByEmail erro (tratado como não existente):', error);
            return of([]);
          })
        )
      );

      if (existingUsers && existingUsers.length > 0) {
        this.errors.email = this.t('manageUsers.emailExists') || 'Este email já está registado.';
        return false;
      }

      return true;

    } catch (e) {
      console.error('Erro fatal ao verificar unicidade do email:', e);
      this.errors.email = this.t('validation.serverErrorEmail') || 'Erro ao comunicar com o servidor para verificar o email.';
      return false;
    }
  }

  // --- Funções de Edição/Criação ---

  selectUser(user: User) {
    this.messageSuccess = '';
    this.messageError = '';
    this.errors = {};
    this.editingUser = true;
    this.originalEmail = user.email; // Salva o email original

    // Preenche o formulário
    this.userForm = {
      email: user.email,
      name: user.name,
      role: user.role
    };

    // Scroll para o formulário
    if (isPlatformBrowser(this.platformId)) {
      document.getElementById('userFormSection')?.scrollIntoView({ behavior: 'smooth' });
    }
  }

  async saveUser() {
    this.messageSuccess = '';
    this.messageError = '';
    this.errors = {}; // Limpa erros de validação anteriores

    const form = this.userForm;
    const isEditing = this.editingUser;

    // 1. Validação local (Email Format/Unicidade e Nome)
    const isEmailValid = await this.validateEmail(form.email, isEditing);
    const isNameValid = form.name.trim().length > 0;

    if (!isNameValid) {
      this.errors.name = this.t('manageUsers.nameRequired') || 'O nome é obrigatório.';
    }

    if (!isEmailValid || !isNameValid) {
      // Falha na validação local, não prossegue e mostra erros.
      this.messageError = this.t('common.formValidationFailed') || 'Por favor, corrija os erros no formulário.';
      this.displayMessage(this.messageError);
      this.cdr.detectChanges();
      return;
    }

    this.isSaving = true;

    if (isEditing) {
      // 2. EDICÃO (Atualiza a Role)
      const roleToUpdate = form.role;

      this.adminService.updateUserRole(this.originalEmail!, roleToUpdate).subscribe({
        next: () => {
          const successMsg = this.t('manageUsers.updateSuccessActivation') || 'Usuário atualizado; email de ativação enviado com sucesso.';
          this.displayMessage(successMsg); // ALERTA DE SUCESSO
          this.messageSuccess = successMsg;
          this.resetForm(); // LIMPAR FORMULÁRIO EM SUCESSO
          this.loadUsers();
          this.isSaving = false;
        },
        error: err => {
          console.error(err);
          const serverError = err?.error?.error || err?.error?.message;
          this.messageError = (this.t('manageUsers.updateError') || 'Erro ao atualizar o usuário.') + (serverError ? ` Detalhes: ${serverError}` : '');
          this.displayMessage(this.messageError); // ALERTA DE ERRO GERAL
          // Manter o formulário em caso de erro
          this.isSaving = false;
        }
      });
    } else {
      // 3. CRIAÇÃO (Cria novo usuário)
      const createDto = {
        email: form.email,
        name: form.name,
        role: form.role
      };

      this.adminService.createUser(createDto).subscribe({
        next: () => {
          const successMsg = this.t('manageUsers.createSuccessActivation') || 'Usuário criado; email de ativação enviado com sucesso.';
          this.displayMessage(successMsg); // ALERTA DE SUCESSO
          this.messageSuccess = successMsg;
          this.resetForm(); // LIMPAR FORMULÁRIO EM SUCESSO
          this.loadUsers();
          this.isSaving = false;
        },
        error: err => {
          console.error(err);
          const serverError = err?.error?.error || err?.error?.message;
          this.messageError = (this.t('manageUsers.createError') || 'Erro ao criar usuário.') + (serverError ? ` Detalhes: ${serverError}` : '');
          this.displayMessage(this.messageError); // ALERTA DE ERRO GERAL

          // Se for erro de unicidade que passou na validação local (ex: race condition), mostra o erro no campo.
          if (serverError && (serverError.toLowerCase().includes('already registered') || serverError.toLowerCase().includes('user exists'))) {
            this.errors.email = this.t('manageUsers.emailExists') || 'Este email já está registado.';
          }
          // Manter o formulário em caso de erro
          this.isSaving = false;
        }
      });
    }
  }

  /**
   * Limpa o formulário e redefine o estado para "Criar".
   */
  resetForm() {
    this.editingUser = false;
    this.userForm = this.getInitialForm();
    this.errors = {};
    this.originalEmail = undefined;
    // O messageSuccess/messageError são limpos antes do próximo save, mas mantidos visíveis após um save.
    // Para cumprir o requisito de só limpar o form (não as mensagens), deixamos de limpar aqui.
    // this.messageError = '';
    // this.messageSuccess = '';
    if (this.userNgForm) {
      this.userNgForm.resetForm(this.userForm);
    }
  }
}
