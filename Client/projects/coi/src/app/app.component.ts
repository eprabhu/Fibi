import { Component } from '@angular/core';
import { NavigationService } from './common/services/navigation.service';

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent {

  constructor(private _navigationService : NavigationService) {}

  title = 'coi';
}
