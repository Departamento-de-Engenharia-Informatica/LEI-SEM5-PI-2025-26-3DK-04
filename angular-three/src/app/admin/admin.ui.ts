import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterLink } from '@angular/router';

@Component({
  selector: 'app-admin-ui',
  standalone: true,
  imports: [CommonModule, RouterLink],
  templateUrl: './admin.ui.html',
  styleUrls: ['./admin.ui.scss']
})
export class AdminUI {
  sections = [
    { name: 'User Management'},
    { name: 'System Logs'},
    { name: 'Settings'},
  ];
}
