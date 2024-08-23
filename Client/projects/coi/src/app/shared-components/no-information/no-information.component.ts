import { ChangeDetectionStrategy, Component, EventEmitter, Input, Output } from '@angular/core';
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
  @Input() canShowAddButton = false;
  @Input() buttonName = '';
  @Output() buttonAction = new EventEmitter<any>();

  emitButtonEvent(){
    this.buttonAction.emit(true);
  }
}
