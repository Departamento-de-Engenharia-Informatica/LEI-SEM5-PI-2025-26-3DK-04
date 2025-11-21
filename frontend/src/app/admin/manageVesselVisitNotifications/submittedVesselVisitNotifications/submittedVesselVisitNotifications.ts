import { Component, OnInit, ChangeDetectorRef, Output, EventEmitter } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { TranslationService } from '../../../translation.service';
import { AdminService } from '../../admin.service';

@Component({
  selector: 'app-submitted-vessel-visit-notifications',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './submittedVesselVisitNotifications.html',
  styleUrls: ['./submittedVesselVisitNotifications.scss']
})
export class SubmittedVesselVisitNotifications implements OnInit {

  // Evento para notificar o componente pai sobre mudanças (ex: para atualizar a lista Withdrawn)
  @Output() notificationsChanged = new EventEmitter<void>();

  // Listas de Notificações
  submittedNotifications: any[] = [];
  selectedNotification: any = null;

  // Dados de Apoio (Dropdowns)
  docks: any[] = [];

  // Review modal state (APPROVE/REJECT)
  showReviewModal: boolean = false;
  reviewAction: 'approve' | 'reject' | null = null;
  reviewNotification: any = null;
  reviewForm: any = {
    dockId: null,
    officerId: '',
    reason: ''
  };

  constructor(
    private adminService: AdminService,
    private translation: TranslationService,
    private cdr: ChangeDetectorRef
  ) { }

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
       INITIALIZATION / LOAD DATA
  ------------------- */

  loadData() {
    this.loadDocks();
    this.loadSubmittedNotifications();
  }

  loadDocks() {
    this.adminService.getAllDocks()?.subscribe({
      next: res => { this.docks = res || []; this.cdr.detectChanges(); }
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

  loadNotificationDetails(n: any) {
    this.adminService.getVesselVisitNotificationById(n.id).subscribe({
      next: res => {
        this.selectedNotification = res;
        this.cdr.detectChanges();
      }
    });
  }

  /* -------------------
       REVIEW (APPROVE/REJECT)
  ------------------- */

  openReviewModal(notification: any, action: 'approve' | 'reject') {
    this.reviewNotification = notification;
    this.reviewAction = action;
    this.showReviewModal = true;
    this.reviewForm = {
      dockId: notification.dockId || null,
      officerId: '',
      reason: ''
    };
  }

  closeReviewModal() {
    this.showReviewModal = false;
    this.reviewNotification = null;
    this.reviewAction = null;
    this.reviewForm = { dockId: null, officerId: '', reason: '' };
  }

  confirmReview() {
    if (!this.reviewNotification || !this.reviewAction) return;

    if (this.reviewAction === 'approve') {
      if (!this.reviewForm.dockId) {
        return this.displayMessage(this.t('vesselVisitNotifications.dockRequired'));
      }
      if (!this.reviewForm.officerId) {
        return this.displayMessage(this.t('vesselVisitNotifications.officerRequired'));
      }

      this.adminService.approveVesselVisitNotification(
        this.reviewNotification.id,
        this.reviewForm.dockId,
        this.reviewForm.officerId
      ).subscribe({
        next: () => {
          this.displayMessage(this.t('vesselVisitNotifications.approveSuccess'));
          this.closeReviewModal();
          this.loadSubmittedNotifications(); // Recarrega a lista
          this.notificationsChanged.emit(); // Notifica o pai (se houver)
        },
        error: err => {
          this.displayMessage(this.t('vesselVisitNotifications.approveError') + ': ' + (err?.error?.Message || err?.message || err));
        }
      });
    } else if (this.reviewAction === 'reject') {
      if (!this.reviewForm.reason) {
        return this.displayMessage(this.t('vesselVisitNotifications.reasonRequired'));
      }
      if (!this.reviewForm.officerId) {
        return this.displayMessage(this.t('vesselVisitNotifications.officerRequired'));
      }

      this.adminService.rejectVesselVisitNotification(
        this.reviewNotification.id,
        this.reviewForm.reason,
        this.reviewForm.officerId
      ).subscribe({
        next: () => {
          this.displayMessage(this.t('vesselVisitNotifications.rejectSuccess'));
          this.closeReviewModal();
          this.loadSubmittedNotifications(); // Recarrega a lista
          this.notificationsChanged.emit(); // Notifica o pai (se houver)
        },
        error: err => {
          this.displayMessage(this.t('vesselVisitNotifications.rejectError') + ': ' + (err?.error?.Message || err?.message || err));
        }
      });
    }
  }

  /* -------------------
       AÇÃO: Withdraw
  ------------------- */

  // AÇÃO: Withdraw (de Submitted para Withdrawn)
  withdrawNotification(notification: any) {
    if (!confirm(this.t('vesselVisitNotifications.confirmWithdraw'))) return;

    this.adminService.withdrawVesselVisitNotification(notification.id).subscribe({
      next: () => {
        this.displayMessage(this.t('vesselVisitNotifications.withdrawSuccess'));
        this.loadSubmittedNotifications(); // Notificação removida desta lista
        this.notificationsChanged.emit(); // Notifica o pai para atualizar a lista Withdrawn
      },
      error: err => this.displayMessage(this.t('vesselVisitNotifications.withdrawError') + ': ' + (err?.error?.message || err))
    });
  }
}
