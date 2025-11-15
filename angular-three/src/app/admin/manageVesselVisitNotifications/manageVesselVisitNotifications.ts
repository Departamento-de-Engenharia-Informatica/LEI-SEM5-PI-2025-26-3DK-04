import { Component, OnInit, ChangeDetectorRef } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { TranslationService } from '../../translation.service';
import { AdminService } from '../admin.service';

@Component({
  selector: 'app-manage-vessel-visit-notifications',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './manageVesselVisitNotifications.html',
  styleUrls: ['./manageVesselVisitNotifications.scss']
})
export class ManageVesselVisitNotifications implements OnInit {
  vessels: any[] = [];
  docks: any[] = [];
  physicalResources: any[] = [];
  staff: any[] = [];
  representatives: any[] = [];

  submittedNotifications: any[] = [];
  selectedNotification: any = null;
  
  // Review modal state
  showReviewModal: boolean = false;
  reviewAction: 'approve' | 'reject' | null = null;
  reviewNotification: any = null;
  reviewForm: any = {
    dockId: null,
    officerId: '',
    reason: ''
  };

  // Form model aligned with CreateNotificationDto
  form: any = {
    vesselId: null,
    representativeId: '',
    loadingManifests: [] as any[],
    unloadingManifests: [] as any[],
    crew: [] as any[],
    arrivalTime: '', // datetime-local string
    departureTime: '', // datetime-local string
    staffMemberIds: [] as string[],
    physicalResourceId: null,
    dockId: null
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
    this.loadVessels();
    this.loadDocks();
    this.loadPhysicalResources();
    this.loadStaff();
    this.loadRepresentatives();
    this.loadSubmittedNotifications();
    this.resetForm();
  }

  /* -------------------
     LOADING DATA
  ------------------- */
  loadVessels() {
    this.adminService.getVessels()?.subscribe({
      next: (res: any[]) => { this.vessels = res || []; this.cdr.detectChanges(); },
      error: err => { console.error('Error loading vessels', err); alert('Error loading vessels: ' + (err?.message || err)); }
    });
  }

  loadDocks() {
    this.adminService.getAllDocks()?.subscribe({
      next: (res: any[]) => { this.docks = res || []; this.cdr.detectChanges(); },
      error: err => { console.error('Error loading docks', err); alert('Error loading docks: ' + (err?.message || err)); }
    });
  }

  loadPhysicalResources() {
    this.adminService.getAllPhysicalResources().subscribe({
      next: (res: any[]) => { this.physicalResources = res || []; this.cdr.detectChanges(); },
      error: err => { console.error('Error loading physical resources', err); }
    });
  }

  loadStaff() {
    this.adminService.getAllStaffMembers().subscribe({
      next: (res: any[]) => { this.staff = res || []; this.cdr.detectChanges(); },
      error: err => { console.error('Error loading staff members', err); }
    });
  }

  loadRepresentatives() {
    this.adminService.getAllRepresentatives().subscribe({
      next: (res: any[]) => { this.representatives = res || []; this.cdr.detectChanges(); },
      error: err => { console.error('Error loading representatives', err); }
    });
  }

  loadSubmittedNotifications() {
    if (this.adminService.getSubmittedVesselVisitNotifications) {
      this.adminService.getSubmittedVesselVisitNotifications().subscribe({
        next: (res: any[]) => { this.submittedNotifications = res || []; this.cdr.detectChanges(); },
        error: err => { console.error('Error loading notifications', err); alert('Error loading notifications: ' + (err?.message || err)); }
      });
    }
  }
  submitNotification(notification: any) {
    this.adminService.submitVesselVisitNotification(notification.id).subscribe({
      next: () => {
        alert('Notification submitted successfully!');
        this.loadSubmittedNotifications();
      },
      error: err => {
        console.error('Error submitting notification', err);
        alert('Error: ' + (err?.error?.message || err));
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
        alert(this.t('vesselVisitNotifications.dockRequired'));
        return;
      }
      if (!this.reviewForm.officerId) {
        alert(this.t('vesselVisitNotifications.officerRequired'));
        return;
      }

      this.adminService.approveVesselVisitNotification(
        this.reviewNotification.id,
        this.reviewForm.dockId,
        this.reviewForm.officerId
      ).subscribe({
        next: () => {
          alert(this.t('vesselVisitNotifications.approveSuccess'));
          this.closeReviewModal();
          this.loadSubmittedNotifications();
        },
        error: err => {
          console.error('Error approving notification', err);
          alert(this.t('vesselVisitNotifications.approveError') + ': ' + (err?.error?.Message || err?.message || err));
        }
      });
    } else if (this.reviewAction === 'reject') {
      if (!this.reviewForm.reason) {
        alert(this.t('vesselVisitNotifications.reasonRequired'));
        return;
      }
      if (!this.reviewForm.officerId) {
        alert(this.t('vesselVisitNotifications.officerRequired'));
        return;
      }

      this.adminService.rejectVesselVisitNotification(
        this.reviewNotification.id,
        this.reviewForm.reason,
        this.reviewForm.officerId
      ).subscribe({
        next: () => {
          alert(this.t('vesselVisitNotifications.rejectSuccess'));
          this.closeReviewModal();
          this.loadSubmittedNotifications();
        },
        error: err => {
          console.error('Error rejecting notification', err);
          alert(this.t('vesselVisitNotifications.rejectError') + ': ' + (err?.error?.Message || err?.message || err));
        }
      });
    }
  }


  loadNotificationDetails(n: any) {
    if (this.adminService.getVesselVisitNotificationById) {
      this.adminService.getVesselVisitNotificationById(n.id).subscribe({
        next: (res: any) => { this.selectedNotification = res; this.cdr.detectChanges(); },
        error: err => { console.error('Error fetching notification details', err); alert('Error fetching notification details: ' + (err?.message || err)); }
      });
    } else {
      this.selectedNotification = n;
    }
  }

  /* -------------------
     FORM HELPERS: MANIFESTS / CONTAINERS / CREW
  ------------------- */
  resetForm() {
    this.form = {
      vesselId: null,
      representativeId: '',
      loadingManifests: [{ containers: [{ payloadWeight: 0, contentsDescription: '' }] }],
      unloadingManifests: [{ containers: [{ payloadWeight: 0, contentsDescription: '' }] }],
      crew: [],
      arrivalTime: '',
      departureTime: '',
      staffMemberIds: [],
      physicalResourceId: null,
      dockId: null
    };
    this.cdr.detectChanges();
  }

  addLoadingManifest() { this.form.loadingManifests.push({ containers: [{ payloadWeight: 0, contentsDescription: '' }] }); }
  removeLoadingManifest(i: number) { this.form.loadingManifests.splice(i, 1); }
  addLoadingContainer(i: number) { this.form.loadingManifests[i].containers.push({ payloadWeight: 0, contentsDescription: '' }); }
  removeLoadingContainer(mi: number, ci: number) { this.form.loadingManifests[mi].containers.splice(ci, 1); }

  addUnloadingManifest() { this.form.unloadingManifests.push({ containers: [{ payloadWeight: 0, contentsDescription: '' }] }); }
  removeUnloadingManifest(i: number) { this.form.unloadingManifests.splice(i, 1); }
  addUnloadingContainer(mi: number) { this.form.unloadingManifests[mi].containers.push({ payloadWeight: 0, contentsDescription: '' }); }
  removeUnloadingContainer(mi: number, ci: number) { this.form.unloadingManifests[mi].containers.splice(ci, 1); }

  addCrew() { this.form.crew.push({ name: '', citizenId: '', nationality: '' }); }
  removeCrew(i: number) { this.form.crew.splice(i, 1); }

  onStaffChange(event: any) {
    const id = event.target.value;
    if (event.target.checked) {
      if (!this.form.staffMemberIds.includes(id)) this.form.staffMemberIds.push(id);
    } else {
      this.form.staffMemberIds = this.form.staffMemberIds.filter((s: any) => s !== id);
    }
  }

  /* -------------------
     CREATE / SUBMIT
  ------------------- */
  createNotification() {
    try {
      if (!this.form.vesselId) return alert('Vessel is required');
      if (!this.form.representativeId) return alert('Representative is required');
      if (!this.form.arrivalTime || !this.form.departureTime) return alert('Arrival and Departure times are required');

      const dto = {
        VesselId: this.form.vesselId,
        RepresentativeId: this.form.representativeId,
        LoadingManifests: this.form.loadingManifests.map((m: any) => ({ Containers: m.containers.map((c: any) => ({ PayloadWeight: Number(c.payloadWeight), ContentsDescription: c.contentsDescription || '' })) })),
        UnloadingManifests: this.form.unloadingManifests.map((m: any) => ({ Containers: m.containers.map((c: any) => ({ PayloadWeight: Number(c.payloadWeight), ContentsDescription: c.contentsDescription || '' })) })),
        Crew: this.form.crew.map((cr: any) => ({ Name: cr.name, CitizenId: cr.citizenId, Nationality: cr.nationality })),
        ArrivalTime: new Date(this.form.arrivalTime).toISOString(),
        DepartureTime: new Date(this.form.departureTime).toISOString(),
        StaffMemberIds: this.form.staffMemberIds,
        PhysicalResourceId: this.form.physicalResourceId,
        DockId: this.form.dockId
      };

      if (!this.adminService.createVesselVisitNotification) return alert('Create service not available.');

      this.adminService.createVesselVisitNotification(dto).subscribe({
        next: () => { alert('Notification created successfully'); this.loadSubmittedNotifications(); this.resetForm(); },
        error: err => { console.error('Error creating notification', err); alert('Error: ' + (err?.error?.message || err)); }
      });
    } catch (ex) {
      console.error('Exception creating notification', ex);
      alert('Unexpected error: ' + ex);
    }
  }
}
