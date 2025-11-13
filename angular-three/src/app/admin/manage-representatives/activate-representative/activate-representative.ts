import { Component, Inject, OnInit, PLATFORM_ID } from '@angular/core';
import { CommonModule, isPlatformBrowser } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { AdminService } from '../../admin.service';
import { TranslationService } from '../../../translation.service';
import { Router } from '@angular/router';

@Component({
  selector: 'app-activate-representative',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './activate-representative.html',
  styleUrls: ['./activate-representative.scss']
})
export class ActivateRepresentative implements OnInit {
  representatives: any[] = [];
  filteredRepresentatives: any[] = [];
  searchTerm = '';
  selectedId = '';     // binding para o input
  message = '';        // mensagens de sucesso/erro
  currentLang = 'en';

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

  t(key: string) {
    return this.translation.translate(key);
  }

  ngOnInit() {
    this.loadRepresentatives();
  }

  loadRepresentatives() {
    this.adminService.getInactiveRepresentatives().subscribe({
      next: reps => {
        // garantir que o campo 'status' está lá e normalizar a propriedade isActive
        this.representatives = (reps || []).map(r => ({
          ...r,
          isActive: (r.status || '').toString().toLowerCase() === 'active'
        }));
        this.applyFilter();
      },
      error: err => {
        console.error('Load inactive reps error', err);
        this.message = this.t('manageRepresentatives.loadError');
      }
    });
  }

  applyFilter() {
    const q = (this.searchTerm || '').toLowerCase();
    this.filteredRepresentatives = this.representatives.filter(r =>
      (r.name || '').toLowerCase().includes(q) ||
      (r.email || '').toLowerCase().includes(q) ||
      ((r.citizenId ?? r.id) || '').toString().toLowerCase().includes(q)
    );
  }

  activateRepresentative(id: string) {
    if (!id) {
      this.message = this.t('manageRepresentatives.invalidId') || 'Please enter a valid ID.';
      return;
    }

    this.adminService.activateRepresentative(id).subscribe({
      next: () => {
        this.message = this.t('manageRepresentatives.activateSuccess');
        this.selectedId = '';
        this.loadRepresentatives();
      },
      error: err => {
        console.error('Activate error', err);
        this.message = this.t('manageRepresentatives.activateError');
      }
    });
  }

  goBack() {
    this.router.navigate(['/admin/manage-representatives']);
  }
}
