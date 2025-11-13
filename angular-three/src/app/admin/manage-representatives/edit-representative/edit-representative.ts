import { Component, Inject, PLATFORM_ID } from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TranslationService } from '../../../translation.service';
import { AdminService } from '../../admin.service';
import {Router} from '@angular/router';

@Component({
  selector: 'app-edit-representative',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './edit-representative.html',
  styleUrls: ['./edit-representative.scss']
})
export class EditRepresentative {
  representatives: any[] = [];
  filteredRepresentatives: any[] = [];
  searchTerm = '';
  selectedId = '';
  repForm: any = null;
  currentLang = 'en';

  constructor(private adminService: AdminService, private translation: TranslationService,private router: Router,@Inject(PLATFORM_ID) private platformId: Object) {
    const isBrowser = isPlatformBrowser(this.platformId);
    if (isBrowser) {
      const savedLang = localStorage.getItem('appLang');
      if (savedLang) this.translation.setLanguage(savedLang);
      this.currentLang = savedLang || 'en';
    }
  }

  t(key: string) { return this.translation.translate(key); }

  ngOnInit() { this.loadRepresentatives(); }

  loadRepresentatives() {
    this.adminService.getAllRepresentatives().subscribe({
      next: res => {
        this.representatives = res;
        this.applyFilter(); // ← IMPORTANTE
      },
      error: err => {
        console.error(err);
        alert(this.t('manageRepresentatives.loadError'));
      }
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

  selectRepresentative() {
    const rep = this.representatives.find(r => (r.citizenId ?? r.id) === this.selectedId);
    if (!rep) return alert('Representative not found');
    this.repForm = { ...rep, citizenId: rep.citizenId ?? rep.id };
  }

  saveChanges() {
    if (!this.repForm) return;
    const id = this.repForm.citizenId ?? this.repForm.id;
    this.adminService.updateRepresentative(id, this.repForm).subscribe({
      next: () => { alert(this.t('manageRepresentatives.updateSuccess')); this.repForm = null; this.selectedId = ''; this.loadRepresentatives(); },
      error: err => { console.error(err); alert(this.t('manageRepresentatives.updateError')); }
    });
  }

  cancelEdit() { this.repForm = null; this.selectedId = ''; }

  goBack() {
    this.router.navigate(['/admin/manage-representatives']);
  }
}
