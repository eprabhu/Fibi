import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { closeSlider, openSlider } from '../../common/utilities/custom-utilities';

@Component({
    selector: 'app-header-sliders',
    templateUrl: './header-sliders.component.html',
    styleUrls: ['./header-sliders.component.scss']
})
export class HeaderSlidersComponent implements OnInit {

    @Input() sliderType: string;
    @Input() coiPersonId: any;
    @Input() personName: any;
    @Input() reviewStatus: any = null;
    @Output() closePage: EventEmitter<any> = new EventEmitter<any>();

    constructor() { }

    ngOnInit() {
        setTimeout(() => {
            openSlider('disclosure-entity-slider-2');
        });
    }

    validateSliderClose() {
        closeSlider('disclosure-entity-slider-2');
        setTimeout(() => {
            this.closePage.emit();
        }, 500);
    }

}
