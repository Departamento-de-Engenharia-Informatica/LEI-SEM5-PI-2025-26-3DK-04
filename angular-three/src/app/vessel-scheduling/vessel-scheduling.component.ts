import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { HttpClient } from '@angular/common/http';

@Component({
  selector: 'app-vessel-scheduling',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './vessel-scheduling.component.html',
  styleUrls: ['./vessel-scheduling.component.scss']
})
export class VesselSchedulingComponent {
  targetDate: string = '';
  schedulingResult: string = '';
  isLoading: boolean = false;
  errorMessage: string = '';

  constructor(private http: HttpClient) {
    // Set today's date as default
    const today = new Date();
    this.targetDate = today.toISOString().split('T')[0];
  }

  calculateSchedule(): void {
    if (!this.targetDate) {
      this.errorMessage = 'Please select a date';
      return;
    }

    this.isLoading = true;
    this.errorMessage = '';
    this.schedulingResult = '';

    const url = `http://localhost:5000/shortest_delay?date=${this.targetDate}`;

    this.http.get(url, { responseType: 'text' }).subscribe({
      next: (response) => {
        this.schedulingResult = response;
        this.isLoading = false;
      },
      error: (error) => {
        this.errorMessage = 'Error calculating schedule: ' + error.message;
        this.isLoading = false;
      }
    });
  }

  clearResults(): void {
    this.schedulingResult = '';
    this.errorMessage = '';
  }
}
