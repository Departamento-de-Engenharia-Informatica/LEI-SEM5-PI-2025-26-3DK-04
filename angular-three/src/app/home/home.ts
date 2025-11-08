import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TranslationService } from '../translation.service';

@Component({
  selector: 'app-home',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './home.html',
  styleUrl: './home.scss',
})
export class Home {
  constructor(private translation: TranslationService) {}

  t(key: string): any {
    return this.translation.translate(key);
  }
}
