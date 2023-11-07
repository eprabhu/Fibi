import {Component, ElementRef, EventEmitter, HostListener, Input, OnInit, Output} from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Component({
    selector: 'app-coi-slider',
    templateUrl: './coi-slider.component.html',
    styleUrls: ['./coi-slider.component.scss']
})
export class CoiSliderComponent implements OnInit {

    @Input() sliderName = 'coi-slider';
    @Input() slider_z_index: any = 51;
    @Input() overlay_z_index: any = 50;
    @Input() isStickyNeeded = true;
    @Output() closeSlider: EventEmitter<undefined> = new EventEmitter<undefined>();
    @Input() isHeaderNeeded = true;

    constructor(private elementRef: ElementRef, private _commonService: CommonService) {}

    emitCloseSlider() {
        this.closeSlider.emit();
    }

    ngOnInit() {
        setTimeout(() => {
            document.getElementById(`${this.sliderName}-overlay`).style.zIndex = this.overlay_z_index;
            document.getElementById(this.sliderName).style.zIndex = this.slider_z_index;
        });
    }

    onWindowScroll(event) {
        const pageYOffset = this.elementRef.nativeElement.querySelector('.slider-container').scrollTop;
        this._commonService.$sliderScrollAction.next({event, pageYOffset});
    }

}
