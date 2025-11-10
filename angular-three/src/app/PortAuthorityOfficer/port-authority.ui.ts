import { Component } from '@angular/core';
import { TranslationService } from '../translation.service';
import { CommonModule } from '@angular/common';
@Component({
  selector: 'app-port-authority-ui',
  standalone: true,
  templateUrl: './port-authority.ui.html',
  styleUrl: './port-authority.ui.scss',
  imports: [CommonModule]
})
export class PortAuthorityUI {constructor(private translation: TranslationService) {}

  t(key: string) {
    return this.translation.translate(key);
  }
}
