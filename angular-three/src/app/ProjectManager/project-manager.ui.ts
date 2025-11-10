import { Component } from '@angular/core';
import { TranslationService } from '../translation.service';
import { CommonModule } from '@angular/common';
import { RouterLink } from '@angular/router';
@Component({
  selector: 'app-project-manager-ui',
  standalone: true,
  templateUrl: './project-manager.ui.html',
  styleUrl: './project-manager.ui.scss',
  imports: [CommonModule, RouterLink]
})
export class ProjectManagerUI {constructor(private translation: TranslationService) {}

  t(key: string) {
    return this.translation.translate(key);
  }
}
