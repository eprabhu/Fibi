import {Component, Input} from '@angular/core';
import {NO_DATA_FOUND_MESSAGE} from "../../app-constants";

@Component({
  selector: 'app-no-data-label',
  templateUrl: './no-data-label.component.html',
  styleUrls: ['./no-data-label.component.scss']
})
export class NoDataLabelComponent {

  @Input() valueToShow: string | number | any[] = '';
  @Input() classesToApply: string = '';
  @Input() customNoDataFoundMessage = NO_DATA_FOUND_MESSAGE;


  isArrayAndEmpty(value) {
    return Array.isArray(value) ? value.length > 0: (value != null || value == 0);
  }
}
