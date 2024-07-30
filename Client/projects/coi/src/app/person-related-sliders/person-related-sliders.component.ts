import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { openCoiSlider } from '../common/utilities/custom-utilities';

@Component({
    selector: 'app-person-related-sliders',
    templateUrl: './person-related-sliders.component.html',
    styleUrls: ['./person-related-sliders.component.scss']
})
export class PersonRelatedSlidersComponent implements OnInit {

    @Input() for: string;
    @Input() sliderType: string;
    @Input() personId: string;
    @Input() personName: string;
    @Input() reviewStatusCode: any = null;
    @Input() isAttachmentViewMode: boolean = true;
    @Output() closePage: EventEmitter<any> = new EventEmitter<any>();

    constructor() { }

    ngOnInit() {
        setTimeout(() => {
            openCoiSlider('disclosure-entity-slider-2');
        });
    }

    validateSliderClose() {
        setTimeout(() => {
            this.closePage.emit();
        }, 500);
    }

}
