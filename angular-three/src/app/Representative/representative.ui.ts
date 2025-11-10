import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TranslationService } from '../translation.service';

@Component({
  standalone: true,
  selector: 'app-representative',
  templateUrl: './representative.ui.html',
  styleUrls: ['./representative.ui.scss'],
  imports: [CommonModule]
})
export class RepresentativeUI {
  constructor(private translation: TranslationService) {}

  t(key: string) {
    return this.translation.translate(key);
  }
}
