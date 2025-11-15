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

  notifications: any[] = [];
  inProgressNotifications: any[] = [];
  submittedNotifications: any[] = [];
  selectedNotification: any = null;

  vessels: any[] = [];
  docks: any[] = [];
  physicalResources: any[] = [];
  staff: any[] = [];
  representatives: any[] = [];

  form: any = {
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
    this.loadInProgressNotifications();
  }

  /* -------------------
       LOAD DATA
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

  loadInProgressNotifications() {
    this.adminService.getInProgressVesselVisitNotifications().subscribe({
      next: res => {
        this.inProgressNotifications = res || [];
        this.cdr.detectChanges();
      },
      error: () => alert('Error loading in-progress notifications')
    });
  }

  loadSubmittedNotifications() {
    this.adminService.getSubmittedVesselVisitNotifications().subscribe({
      next: res => {
        this.submittedNotifications = res || [];
        this.cdr.detectChanges();
      },
      error: () => alert('Error loading submitted notifications')
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
       SUBMIT
  ------------------- */

  submitNotification(notification: any) {
    if (notification.status !== 'InProgress') {
      return alert('Only In Progress notifications can be submitted.');
    }

    this.adminService.submitVesselVisitNotification(notification.id).subscribe({
      next: () => {
        alert('Notification submitted successfully!');
        this.loadInProgressNotifications();
        this.loadSubmittedNotifications();
      },
      error: err => alert('Error: ' + (err?.error?.message || err))
    });
  }

  /* -------------------
       CREATE
  ------------------- */

  createNotification() {
    if (!this.form.vesselId || !this.form.representativeId) {
      return alert('Required fields missing');
    }

    const dto = {
      VesselId: this.form.vesselId,
      RepresentativeId: this.form.representativeId,
      LoadingManifests: this.form.loadingManifests.map((m: any) => ({
        Containers: m.containers.map((c: any) => ({
          PayloadWeight: Number(c.payloadWeight),
          ContentsDescription: c.contentsDescription
        }))
      })),
      UnloadingManifests: this.form.unloadingManifests.map((m: any) => ({
        Containers: m.containers.map((c: any) => ({
          PayloadWeight: Number(c.payloadWeight),
          ContentsDescription: c.contentsDescription
        }))
      })),
      Crew: this.form.crew.map((cr: any) => ({
        Name: cr.name,
        CitizenId: cr.citizenId,
        Nationality: cr.nationality
      })),
      ArrivalTime: new Date(this.form.arrivalTime).toISOString(),
      DepartureTime: new Date(this.form.departureTime).toISOString(),
      StaffMemberIds: this.form.staffMemberIds,
      PhysicalResourceId: this.form.physicalResourceId,
      DockId: this.form.dockId
    };

    this.adminService.createVesselVisitNotification(dto).subscribe({
      next: () => {
        alert('Notification created');
        this.resetForm();
        this.loadInProgressNotifications();
      },
      error: err => alert('Error: ' + (err?.error?.message || err))
    });
  }

  /* -------------------
       FORM HELPERS
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
  }

  onStaffChange(event: any) {
    const id = event.target.value;
    if (event.target.checked) {
      if (!this.form.staffMemberIds.includes(id))
        this.form.staffMemberIds.push(id);
    } else {
      this.form.staffMemberIds = this.form.staffMemberIds.filter((s: any) => s !== id);
    }
  }

  /* -------------------
       Manifest helpers
  ------------------- */

  addLoadingManifest() {
    this.form.loadingManifests.push({ containers: [{ payloadWeight: 0, contentsDescription: '' }] });
  }

  removeLoadingManifest(i: number) {
    this.form.loadingManifests.splice(i, 1);
  }

  addLoadingContainer(mi: number) {
    this.form.loadingManifests[mi].containers.push({ payloadWeight: 0, contentsDescription: '' });
  }

  removeLoadingContainer(mi: number, ci: number) {
    this.form.loadingManifests[mi].containers.splice(ci, 1);
  }

  addUnloadingManifest() {
    this.form.unloadingManifests.push({ containers: [{ payloadWeight: 0, contentsDescription: '' }] });
  }

  removeUnloadingManifest(i: number) {
    this.form.unloadingManifests.splice(i, 1);
  }

  addUnloadingContainer(mi: number) {
    this.form.unloadingManifests[mi].containers.push({ payloadWeight: 0, contentsDescription: '' });
  }

  removeUnloadingContainer(mi: number, ci: number) {
    this.form.unloadingManifests[mi].containers.splice(ci, 1);
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
