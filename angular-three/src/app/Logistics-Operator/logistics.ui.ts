import { Component } from '@angular/core';
import { TranslationService } from '../translation.service';
@Component({
  selector: 'app-logistics-ui',
  standalone: true,
  templateUrl: './logistics.ui.html',
  styleUrl: './logistics.ui.scss'
})
export class LogisticsUI {
  constructor(private translation: TranslationService) {}

  t(key: string) {
    return this.translation.translate(key);
  }
}
