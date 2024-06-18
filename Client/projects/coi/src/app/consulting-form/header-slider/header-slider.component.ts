import { Component, EventEmitter, Input, Output } from '@angular/core';
import { openCoiSlider } from '../../common/utilities/custom-utilities';

@Component({
  selector: 'app-header-slider',
  templateUrl: './header-slider.component.html',
  styleUrls: ['./header-slider.component.scss']
})
export class HeaderSliderComponent {

    @Input() sliderType: string;
    @Input() consultingPersonId: any;
    @Input() consultingPersonName: any;
    @Input() reviewStatus: any = null;
    @Output() closePage: EventEmitter<any> = new EventEmitter<any>();

    constructor() { }

    ngOnInit() {
        setTimeout(() => {
            openCoiSlider('disclosure-header-count-slider');
        });
    }

    validateSliderClose() {
        setTimeout(() => {
            this.closePage.emit();
        }, 500);
    }

}
