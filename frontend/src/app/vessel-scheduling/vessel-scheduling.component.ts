import { Component, ChangeDetectorRef, NgZone, Inject, PLATFORM_ID } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { HttpClient } from '@angular/common/http';
import { isPlatformBrowser } from '@angular/common';
import { TranslationService } from '../translation.service';
import { CONFIG } from '../config';

interface VesselSchedule {
  vessel: string;
  dockId: string;
  dockName?: string;
  arrival: number;
  departure: number;
  startTime: number;
  endTime: number;
  duration: number;
  delay: number;
  cranes?: number; // Number of cranes used (for multi-crane)
}

interface IgnoredNotification {
  vessel: string;
  reason: string;
}

interface ScheduleResponse {
  date: string;
  totalDelay: number;
  schedule: VesselSchedule[];
  ignoredNotifications?: IgnoredNotification[];
}

interface AlgorithmResult {
  algorithm: string;
  totalDelay: number;
  vesselCount: number;
  computationTime: number;
  timestamp: Date;
}

@Component({
  selector: 'app-vessel-scheduling',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './vessel-scheduling.component.html',
  styleUrls: ['./vessel-scheduling.component.scss']
})
export class VesselSchedulingComponent {
  targetDate: string = '';
  algorithm: string = 'shortest_delay'; // 'shortest_delay' or 'heuristic'
  scheduleData: ScheduleResponse | null = null;
  isLoading: boolean = false;
  errorMessage: string = '';
  currentLang = 'en';
  dockNames: { [key: string]: string } = {}; // Cache de nomes dos docks
  
  // Armazenar resultados de ambos algoritmos para comparação
  shortestDelayResult: AlgorithmResult | null = null;
  heuristicResult: AlgorithmResult | null = null;
  showComparison: boolean = false;

  // Multi-crane support
  multiCraneResult: AlgorithmResult | null = null;
  showMultiCraneOption: boolean = false;
  isCalculatingMultiCrane: boolean = false;
  multiCraneScheduleData: ScheduleResponse | null = null;

  constructor(
    private http: HttpClient,
    private cdr: ChangeDetectorRef,
    private ngZone: NgZone,
    private translation: TranslationService,
    @Inject(PLATFORM_ID) private platformId: Object
  ) {
    const today = new Date();
    this.targetDate = today.toISOString().split('T')[0];

    // --- lógica de tradução ---
    const isBrowser = isPlatformBrowser(this.platformId);
    if (isBrowser) {
      const savedLang = localStorage.getItem('appLang');
      if (savedLang) {
        this.translation.setLanguage(savedLang);
        this.currentLang = savedLang;
      }
    }
  }

  // método para usar no template
  translate(key: string) {
    return this.translation.translate(key);
  }

  calculateSchedule(): void {
    if (!this.targetDate) {
      this.errorMessage = this.translate('Please select a date'); // usar tradução
      return;
    }

    this.isLoading = true;
    this.errorMessage = '';
    this.scheduleData = null;
    const startTime = performance.now(); // Medir tempo de computação

    // Escolher endpoint baseado no algoritmo selecionado
    const endpoint = this.algorithm === 'heuristic' ? 'heuristic_schedule' : 'shortest_delay';
    const url = `http://localhost:5003/${endpoint}?date=${this.targetDate}&format=json`;
    console.log('Making request to:', url);

    this.http.get<ScheduleResponse>(url).subscribe({
      next: (response) => {
        console.log('✅ Response received:', response);
        const endTime = performance.now();
        const computationTime = endTime - startTime;
        
        this.ngZone.run(() => {
          // Check if response contains an error
          if ((response as any).error) {
            this.errorMessage = (response as any).error;
            this.scheduleData = null;
            this.isLoading = false;
            console.log('❌ Error from server:', this.errorMessage);
            this.cdr.detectChanges();
          } else {
            this.scheduleData = response;
            
            // Armazenar resultado do algoritmo atual
            const result: AlgorithmResult = {
              algorithm: this.algorithm,
              totalDelay: response.totalDelay,
              vesselCount: response.schedule.length,
              computationTime: computationTime,
              timestamp: new Date()
            };
            
            if (this.algorithm === 'shortest_delay') {
              this.shortestDelayResult = result;
              // Check if there are delays to show multi-crane option
              this.showMultiCraneOption = response.totalDelay > 0;
            } else {
              this.heuristicResult = result;
            }
            
            // Mostrar comparação se ambos algoritmos foram executados para a mesma data
            this.showComparison = this.shortestDelayResult !== null && this.heuristicResult !== null;
            
            // Buscar nomes dos docks
            this.loadDockNames();
            this.isLoading = false;
            console.log('✅ isLoading set to false');
            console.log('✅ scheduleData:', this.scheduleData);
            this.cdr.markForCheck();
            setTimeout(() => this.cdr.detectChanges(), 0);
          }
        });
      },
      error: (error) => {
        console.error('❌ Error occurred:', error);
        this.ngZone.run(() => {
          this.errorMessage = this.translate('Error calculating schedule:') + ' ' + error.message;
          this.isLoading = false;
          this.cdr.detectChanges();
        });
      }
    });
  }

  clearResults(): void {
    this.scheduleData = null;
    this.errorMessage = '';
    this.shortestDelayResult = null;
    this.heuristicResult = null;
    this.showComparison = false;
    this.showMultiCraneOption = false;
    this.multiCraneResult = null;
    this.multiCraneScheduleData = null;
  }

  getDelayClass(delay: number): string {
    if (delay === 0) return 'no-delay';
    if (delay <= 2) return 'low-delay';
    return 'high-delay';
  }

  getStatusIcon(delay: number): string {
    return delay === 0 ? '✓' : '⚠';
  }

  loadDockNames(): void {
    if (!this.scheduleData) return;

    // Extrair dockIds únicos
    const uniqueDockIds = [...new Set(this.scheduleData.schedule.map(v => v.dockId))];

    // Buscar nomes dos docks da API
    this.http.get<any[]>(`${CONFIG.apiUrl}/Dock`).subscribe({
      next: (docks) => {
        // Criar mapa de id -> nome
        docks.forEach(dock => {
          this.dockNames[dock.id] = dock.name;
        });
        // Atualizar scheduleData com nomes
        if (this.scheduleData) {
          this.scheduleData.schedule.forEach(vessel => {
            vessel.dockName = this.dockNames[vessel.dockId] || vessel.dockId;
          });
        }
        this.cdr.detectChanges();
      },
      error: (err) => {
        console.error('Error loading dock names:', err);
        // Se falhar, usar o ID como fallback
        if (this.scheduleData) {
          this.scheduleData.schedule.forEach(vessel => {
            vessel.dockName = vessel.dockId;
          });
        }
        this.cdr.detectChanges();
      }
    });
  }

  getDockName(dockId: string): string {
    return this.dockNames[dockId] || dockId;
  }

  getDelayDifference(): number {
    if (!this.shortestDelayResult || !this.heuristicResult) return 0;
    return this.heuristicResult.totalDelay - this.shortestDelayResult.totalDelay;
  }

  getTimeDifference(): number {
    if (!this.shortestDelayResult || !this.heuristicResult) return 0;
    return this.shortestDelayResult.computationTime - this.heuristicResult.computationTime;
  }

  getDelayPercentageDifference(): number {
    if (!this.shortestDelayResult || !this.heuristicResult || this.shortestDelayResult.totalDelay === 0) return 0;
    const diff = this.heuristicResult.totalDelay - this.shortestDelayResult.totalDelay;
    return (diff / this.shortestDelayResult.totalDelay) * 100;
  }

  // Multi-crane methods
  calculateMultiCraneSchedule(): void {
    if (!this.targetDate) {
      this.errorMessage = this.translate('Please select a date');
      return;
    }

    this.isCalculatingMultiCrane = true;
    this.errorMessage = '';
    const startTime = performance.now();

    const url = `http://localhost:5003/multi_crane_schedule?date=${this.targetDate}&format=json`;
    console.log('Making multi-crane request to:', url);

    this.http.get<ScheduleResponse>(url).subscribe({
      next: (response) => {
        console.log('✅ Multi-crane response received:', response);
        const endTime = performance.now();
        const computationTime = endTime - startTime;
        
        this.ngZone.run(() => {
          if ((response as any).error) {
            this.errorMessage = (response as any).error;
            this.isCalculatingMultiCrane = false;
            console.log('❌ Error from server:', this.errorMessage);
            this.cdr.detectChanges();
          } else {
            this.multiCraneScheduleData = response;
            
            // Store multi-crane result
            this.multiCraneResult = {
              algorithm: 'multi_crane',
              totalDelay: response.totalDelay,
              vesselCount: response.schedule.length,
              computationTime: computationTime,
              timestamp: new Date()
            };
            
            // Load dock names for multi-crane results
            this.loadDockNamesForMultiCrane();
            this.isCalculatingMultiCrane = false;
            console.log('✅ Multi-crane calculation complete');
            this.cdr.markForCheck();
            setTimeout(() => this.cdr.detectChanges(), 0);
          }
        });
      },
      error: (error) => {
        console.error('❌ Multi-crane error occurred:', error);
        this.ngZone.run(() => {
          this.errorMessage = this.translate('Error calculating multi-crane schedule:') + ' ' + error.message;
          this.isCalculatingMultiCrane = false;
          this.cdr.detectChanges();
        });
      }
    });
  }

  loadDockNamesForMultiCrane(): void {
    if (!this.multiCraneScheduleData) return;

    const uniqueDockIds = [...new Set(this.multiCraneScheduleData.schedule.map(v => v.dockId))];

    this.http.get<any[]>(`${CONFIG.apiUrl}/Dock`).subscribe({
      next: (docks) => {
        docks.forEach(dock => {
          this.dockNames[dock.id] = dock.name;
        });
        if (this.multiCraneScheduleData) {
          this.multiCraneScheduleData.schedule.forEach(vessel => {
            vessel.dockName = this.dockNames[vessel.dockId] || vessel.dockId;
          });
        }
        this.cdr.detectChanges();
      },
      error: (err) => {
        console.error('Error loading dock names for multi-crane:', err);
        if (this.multiCraneScheduleData) {
          this.multiCraneScheduleData.schedule.forEach(vessel => {
            vessel.dockName = vessel.dockId;
          });
        }
        this.cdr.detectChanges();
      }
    });
  }

  getMultiCraneDelayImprovement(): number {
    if (!this.shortestDelayResult || !this.multiCraneResult) return 0;
    return this.shortestDelayResult.totalDelay - this.multiCraneResult.totalDelay;
  }

  getMultiCraneDelayPercentageImprovement(): number {
    if (!this.shortestDelayResult || !this.multiCraneResult || this.shortestDelayResult.totalDelay === 0) return 0;
    const improvement = this.shortestDelayResult.totalDelay - this.multiCraneResult.totalDelay;
    return (improvement / this.shortestDelayResult.totalDelay) * 100;
  }

  getTotalCranesUsed(): number {
    if (!this.multiCraneScheduleData) return 0;
    return this.multiCraneScheduleData.schedule.reduce((sum, vessel) => sum + (vessel.cranes || 1), 0);
  }

  getMultiCraneVesselsCount(): number {
    if (!this.multiCraneScheduleData) return 0;
    return this.multiCraneScheduleData.schedule.filter(v => (v.cranes || 1) > 1).length;
  }
}
