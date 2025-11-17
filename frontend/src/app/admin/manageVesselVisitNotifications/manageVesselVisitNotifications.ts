import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { TranslationService } from '../../translation.service';
import { AdminService } from '../admin.service';
import { Observable } from 'rxjs';
// Interfaces para tipagem dos dados
interface Container {
  id?: string;
  payloadWeight: number;
  contentsDescription: string;
}

interface Manifest {
  id?: string;
  containers: Container[];
}

interface CrewMember {
  name: string;
  citizenId: string;
  nationality: string;
}

interface NotificationForm {
  id?: string; // ID da notificação se estiver a editar
  vesselId: string | null;
  representativeId: string;
  loadingManifests: Manifest[];
  unloadingManifests: Manifest[];
  crew: CrewMember[];
  arrivalTime: string;
  departureTime: string;
  staffMemberIds: string[];
  physicalResourceIds: string[];
  dockId: string | null;
}

@Component({
  selector: 'app-manage-vessel-visit-notifications',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './manageVesselVisitNotifications.html',
  styleUrls: ['./manageVesselVisitNotifications.scss']
})
export class ManageVesselVisitNotifications implements OnInit {

  // Listas de Notificações
  notifications: any[] = [];
  inProgressNotifications: any[] = [];
  submittedNotifications: any[] = [];
  withdrawnNotifications: any[] = []; // NOVO: Lista de retiradas
  selectedNotification: any = null;

  // Dados de Apoio (Dropdowns, Checkboxes)
  vessels: any[] = [];
  docks: any[] = [];
  physicalResources: any[] = [];
  staff: any[] = [];
  representatives: any[] = [];

  // Gestão do Formulário
  editing: boolean = false; // NOVO: Flag para alternar entre Create e Edit
  isSaving: boolean = false; // Flag para prevenir duplo clique

  form: NotificationForm;

  constructor(
    private adminService: AdminService,
    private translation: TranslationService,
    private cdr: ChangeDetectorRef
  ) {
    this.form = this.getInitialForm();
  }

  t(key: string) {
    return this.translation.translate(key);
  }

  displayMessage(msg: string) {
    if (typeof window !== 'undefined') alert(msg);
    else console.log('MSG:', msg);
  }

  ngOnInit() {
    this.loadData();
  }

  /* -------------------
       INITIALIZATION
  ------------------- */

  loadData() {
    this.loadVessels();
    this.loadDocks();
    this.loadPhysicalResources();
    this.loadStaff();
    this.loadRepresentatives();
    this.loadAllNotifications();
  }

  getInitialForm(): NotificationForm {
    return {
      vesselId: null,
      representativeId: '',
      loadingManifests: [{ containers: [{ payloadWeight: 0, contentsDescription: '' }] }],
      unloadingManifests: [{ containers: [{ payloadWeight: 0, contentsDescription: '' }] }],
      crew: [],
      arrivalTime: '',
      departureTime: '',
      staffMemberIds: [],
      physicalResourceIds: [],
      dockId: null
    };
  }

  /* -------------------
       LOAD UTILS
  ------------------- */

  loadVessels() {
    this.adminService.getVessels()?.subscribe({
      next: res => { this.vessels = res || []; this.cdr.detectChanges(); }
    });
  }

  loadDocks() {
    this.adminService.getAllDocks()?.subscribe({
      next: res => { this.docks = res || []; this.cdr.detectChanges(); }
    });
  }

  loadPhysicalResources() {
    this.adminService.getAllPhysicalResources().subscribe({
      next: res => { this.physicalResources = res || []; this.cdr.detectChanges(); }
    });
  }

  loadStaff() {
    this.adminService.getAllStaffMembers().subscribe({
      next: res => { this.staff = res || []; this.cdr.detectChanges(); }
    });
  }

  loadRepresentatives() {
    this.adminService.getAllRepresentatives().subscribe({
      next: res => { this.representatives = res || []; this.cdr.detectChanges(); }
    });
  }

  /* -------------------
       LOAD NOTIFICATIONS
  ------------------- */

  loadAllNotifications() {
    this.loadInProgressNotifications();
    this.loadSubmittedNotifications();
    this.loadWithdrawnNotifications(); // NOVO
  }

  loadInProgressNotifications() {
    this.adminService.getInProgressVesselVisitNotifications().subscribe({
      next: res => {
        this.inProgressNotifications = res || [];
        this.cdr.detectChanges();
      },
      error: () => this.displayMessage(this.t('vesselVisitNotifications.loadErrorInProgress') || 'Error loading in-progress notifications')
    });
  }

  loadSubmittedNotifications() {
    this.adminService.getSubmittedVesselVisitNotifications().subscribe({
      next: res => {
        this.submittedNotifications = res || [];
        this.cdr.detectChanges();
      },
      error: () => this.displayMessage(this.t('vesselVisitNotifications.loadErrorSubmitted') || 'Error loading submitted notifications')
    });
  }

  // NOVO: Carregar notificações com status Withdrawn
  loadWithdrawnNotifications() {
    this.adminService.getWithdrawnVesselVisitNotifications().subscribe({
      next: res => {
        this.withdrawnNotifications = res || [];
        this.cdr.detectChanges();
      },
      error: () => this.displayMessage(this.t('vesselVisitNotifications.loadErrorWithdrawn') || 'Error loading withdrawn notifications')
    });
  }

  loadNotificationDetails(n: any) {
    this.adminService.getVesselVisitNotificationById(n.id).subscribe({
      next: res => {
        this.selectedNotification = res;
        this.cdr.detectChanges();
      }
    });
  }

  /* -------------------
       AÇÕES (SUBMIT, WITHDRAW, RESUME)
  ------------------- */

  // AÇÃO 1: Submeter (de InProgress para Submitted)
  submitNotification(notification: any) {
    if (notification.status !== 'InProgress') {
      return this.displayMessage(this.t('vesselVisitNotifications.submitOnlyInProgress'));
    }

    this.adminService.submitVesselVisitNotification(notification.id).subscribe({
      next: () => {
        this.displayMessage(this.t('vesselVisitNotifications.submitSuccess'));
        this.loadAllNotifications();
      },
      error: err => this.displayMessage(this.t('vesselVisitNotifications.submitError') + ': ' + (err?.error?.message || err))
    });
  }

  // AÇÃO 2: Withdraw (de InProgress/Submitted para Withdrawn)
  withdrawNotification(notification: any) {
    if (!confirm(this.t('vesselVisitNotifications.confirmWithdraw'))) return;

    this.adminService.withdrawVesselVisitNotification(notification.id).subscribe({
      next: () => {
        this.displayMessage(this.t('vesselVisitNotifications.withdrawSuccess'));
        this.loadAllNotifications();
      },
      error: err => this.displayMessage(this.t('vesselVisitNotifications.withdrawError') + ': ' + (err?.error?.message || err))
    });
  }

  // AÇÃO 3: Resume (de Withdrawn para InProgress)
  resumeNotification(notification: any) {
    if (notification.status !== 'WithdrawnRequest') {
      return this.displayMessage(this.t('vesselVisitNotifications.resumeOnlyWithdrawn'));
    }

    this.adminService.resumeVesselVisitNotification(notification.id).subscribe({
      next: () => {
        this.displayMessage(this.t('vesselVisitNotifications.resumeSuccess'));
        this.loadAllNotifications();
      },
      error: err => this.displayMessage(this.t('vesselVisitNotifications.resumeError') + ': ' + (err?.error?.message || err))
    });
  }


  /* -------------------
       EDIÇÃO (REQUISITO 1)
  ------------------- */

  // Função para preencher o formulário para edição
  editNotification(notification: any) {
    this.editing = true;
    this.selectedNotification = notification;

    // Formatar ArrivalTime e DepartureTime para o formato local da input type="datetime-local"
    const formatDateTimeLocal = (isoString: string): string => {
      if (!isoString) return '';
      const date = new Date(isoString);
      date.setMinutes(date.getMinutes() - date.getTimezoneOffset());
      return date.toISOString().slice(0, 16);
    };

    // Mapear a notificação para a estrutura do formulário
    this.form = {
      id: notification.id,
      vesselId: notification.vesselId,
      representativeId: notification.representativeId,
      arrivalTime: formatDateTimeLocal(notification.arrivalTime),
      departureTime: formatDateTimeLocal(notification.departureTime),
      staffMemberIds: notification.staffMemberIds || [],
      physicalResourceIds: notification.physicalResourceIds || [],
      dockId: notification.dockId || null,
      crew: notification.crew || [],
      // Mapeamento de Manifests (simplificado para estruturas com ID)
      loadingManifests: notification.loadingCargo?.manifests || [{ containers: [{ payloadWeight: 0, contentsDescription: '' }] }],
      unloadingManifests: notification.unloadingCargo?.manifests || [{ containers: [{ payloadWeight: 0, contentsDescription: '' }] }],
    };

    // Scroll para o formulário
    document.querySelector('.form-section')?.scrollIntoView({ behavior: 'smooth' });
  }


  /* -------------------
       SAVE (CREATE / UPDATE)
  ------------------- */

  createOrUpdateNotification() {
    if (!this.form.vesselId || !this.form.representativeId || this.isSaving) {
      return this.displayMessage(this.t('vesselVisitNotifications.requiredFields'));
    }

    this.isSaving = true;

    const dto = this.mapFormToDto();
    let request$: Observable<any>;
    let successMessage: string;
    let errorMessage: string;

    if (this.editing && this.form.id) {
      // É EDIÇÃO
      request$ = this.adminService.updateVesselVisitNotificationInProgress(this.form.id, dto);
      successMessage = this.t('vesselVisitNotifications.updateSuccess');
      errorMessage = this.t('vesselVisitNotifications.updateError');
    } else {
      // É CRIAÇÃO
      request$ = this.adminService.createVesselVisitNotification(dto);
      successMessage = this.t('vesselVisitNotifications.createSuccess');
      errorMessage = this.t('vesselVisitNotifications.createError');
    }

    request$.subscribe({
      next: () => {
        this.displayMessage(successMessage);
        this.resetForm();
        this.loadAllNotifications();
        this.isSaving = false;
      },
      error: err => {
        this.displayMessage(errorMessage + ': ' + (err?.error?.message || JSON.stringify(err)));
        this.isSaving = false;
        this.cdr.detectChanges();
      }
    });
  }

  private mapFormToDto() {
    return {
      VesselId: this.form.vesselId,
      RepresentativeId: this.form.representativeId,
      // Mapeamento de Manifests: Assegurar que payloadWeight é Number e remover IDs (se existirem)
      LoadingManifests: this.form.loadingManifests.map(m => ({
        Containers: m.containers.map(c => ({
          PayloadWeight: Number(c.payloadWeight),
          ContentsDescription: c.contentsDescription
        }))
      })),
      UnloadingManifests: this.form.unloadingManifests.map(m => ({
        Containers: m.containers.map(c => ({
          PayloadWeight: Number(c.payloadWeight),
          ContentsDescription: c.contentsDescription
        }))
      })),
      Crew: this.form.crew.map(cr => ({
        Name: cr.name,
        CitizenId: cr.citizenId,
        Nationality: cr.nationality
      })),
      ArrivalTime: new Date(this.form.arrivalTime).toISOString(),
      DepartureTime: new Date(this.form.departureTime).toISOString(),
      StaffMemberIds: this.form.staffMemberIds,
      PhysicalResourceIds: this.form.physicalResourceIds,
      DockId: this.form.dockId
    };
  }


  /* -------------------
       FORM HELPERS
  ------------------- */

  resetForm() {
    this.editing = false;
    this.form = this.getInitialForm();
    this.selectedNotification = null;
    this.isSaving = false;
  }

  onStaffChange(event: any) {
    const id = event.target.value;
    if (event.target.checked) {
      if (!this.form.staffMemberIds.includes(id))
        this.form.staffMemberIds.push(id);
    } else {
      this.form.staffMemberIds = this.form.staffMemberIds.filter((s: string) => s !== id);
    }
  }

  onPhysicalResourceChange(event: any) {
    const id = event.target.value;
    if (event.target.checked) {
      if (!this.form.physicalResourceIds.includes(id))
        this.form.physicalResourceIds.push(id);
    } else {
      this.form.physicalResourceIds = this.form.physicalResourceIds.filter((p: string) => p !== id);
    }
  }

  /* -------------------
       Manifest helpers
  ------------------- */

  addLoadingManifest() {
    this.form.loadingManifests.push({ containers: [{ payloadWeight: 0, contentsDescription: '' }] });
  }

  removeLoadingManifest(i: number) {
    if (this.form.loadingManifests.length > 1) {
      this.form.loadingManifests.splice(i, 1);
    }
  }

  addLoadingContainer(mi: number) {
    this.form.loadingManifests[mi].containers.push({ payloadWeight: 0, contentsDescription: '' });
  }

  removeLoadingContainer(mi: number, ci: number) {
    if (this.form.loadingManifests[mi].containers.length > 1) {
      this.form.loadingManifests[mi].containers.splice(ci, 1);
    }
  }

  addUnloadingManifest() {
    this.form.unloadingManifests.push({ containers: [{ payloadWeight: 0, contentsDescription: '' }] });
  }

  removeUnloadingManifest(i: number) {
    if (this.form.unloadingManifests.length > 1) {
      this.form.unloadingManifests.splice(i, 1);
    }
  }

  addUnloadingContainer(mi: number) {
    this.form.unloadingManifests[mi].containers.push({ payloadWeight: 0, contentsDescription: '' });
  }

  removeUnloadingContainer(mi: number, ci: number) {
    if (this.form.unloadingManifests[mi].containers.length > 1) {
      this.form.unloadingManifests[mi].containers.splice(ci, 1);
    }
  }

  /* -------------------
       Crew helpers
  ------------------- */

  addCrew() {
    this.form.crew.push({ name: '', citizenId: '', nationality: '' });
  }

  removeCrew(i: number) {
    this.form.crew.splice(i, 1);
  }
}
