import { Component } from '@angular/core';
import { fadeInOutHeight } from '../../../../../fibi/src/app/common/utilities/animations';

@Component({
  selector: 'app-no-information',
  templateUrl: './no-information.component.html',
  styleUrls: ['./no-information.component.scss'],
  animations: [fadeInOutHeight]
})
export class NoInformationComponent {

}
