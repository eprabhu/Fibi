import { ChangeDetectionStrategy, Component, Input } from '@angular/core';
import { fadeInOutHeight } from '../../common/utilities/animations';

@Component({
  selector: 'app-no-information',
  templateUrl: './no-information.component.html',
  styleUrls: ['./no-information.component.scss'],
  animations: [fadeInOutHeight],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class NoInformationComponent {

  @Input() isBorderNeeded = true;
  @Input() customClass = '';
}
