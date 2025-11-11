import { Component, ChangeDetectorRef, NgZone } from '@angular/core';
import { CommonModule, UpperCasePipe } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { HttpClient } from '@angular/common/http';
import { TranslationService } from '../translation.service';

interface VesselSchedule {
  vessel: string;
  arrival: number;
  departure: number;
  startTime: number;
  endTime: number;
  duration: number;
  delay: number;
}

interface ScheduleResponse {
  date: string;
  totalDelay: number;
  schedule: VesselSchedule[];
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
  scheduleData: ScheduleResponse | null = null;
  isLoading: boolean = false;
  errorMessage: string = '';

  constructor(
    private http: HttpClient, 
    private cdr: ChangeDetectorRef, 
    private ngZone: NgZone,
    private translation: TranslationService
  ) {
    const today = new Date();
    this.targetDate = today.toISOString().split('T')[0];
  }

  translate(key: string): any {
    return this.translation.translate(key);
  }

  calculateSchedule(): void {
    if (!this.targetDate) {
      this.errorMessage = 'Please select a date';
      return;
    }

    this.isLoading = true;
    this.errorMessage = '';
    this.scheduleData = null;

    const url = `http://localhost:5002/shortest_delay?date=${this.targetDate}&format=json`;
    console.log('Making request to:', url);

    this.http.get<ScheduleResponse>(url).subscribe({
      next: (response) => {
        console.log('✅ Response received:', response);
        this.ngZone.run(() => {
          this.scheduleData = response;
          this.isLoading = false;
          console.log('✅ isLoading set to false');
          console.log('✅ scheduleData:', this.scheduleData);
          this.cdr.markForCheck();
          setTimeout(() => this.cdr.detectChanges(), 0);
        });
      },
      error: (error) => {
        console.error('❌ Error occurred:', error);
        this.ngZone.run(() => {
          this.errorMessage = 'Error calculating schedule: ' + error.message;
          this.isLoading = false;
          this.cdr.detectChanges();
        });
      }
    });
  }

  clearResults(): void {
    this.scheduleData = null;
    this.errorMessage = '';
  }

  getDelayClass(delay: number): string {
    if (delay === 0) return 'no-delay';
    if (delay <= 2) return 'low-delay';
    return 'high-delay';
  }

  getStatusIcon(delay: number): string {
    return delay === 0 ? '✓' : '⚠';
  }
}
