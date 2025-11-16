import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterLink } from '@angular/router';
import { TranslationService } from '../translation.service';
@Component({
  selector: 'app-access-denied',
  standalone: true,
  imports: [CommonModule, RouterLink],
  templateUrl: './access-denied.html',
  styleUrls: ['./access-denied.scss']
})
export class AccessDenied {
  constructor(private translation: TranslationService) {}

  t(key: string): any {
    return this.translation.translate(key);
  }
}
