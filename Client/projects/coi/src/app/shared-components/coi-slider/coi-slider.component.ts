import { Component, EventEmitter, Input, Output } from '@angular/core';

@Component({
    selector: 'app-coi-slider',
    templateUrl: './coi-slider.component.html',
    styleUrls: ['./coi-slider.component.scss']
})
export class CoiSliderComponent {

    @Input() sliderName = 'coi-slider';
    @Input() slider_z_index: any = 51;
    @Input() overlay_z_index: any = 50;
    @Input() isHeaderContentAvailable = true;
    @Input() isStickyContentAvailable = true;
    @Output() closeSlider: EventEmitter<undefined> = new EventEmitter<undefined>();

    emitCloseSlider() {
        this.closeSlider.emit();
    }

    ngOnInit() {
        setTimeout(() => {
            document.getElementById(`${this.sliderName}-overlay`).style.zIndex = this.overlay_z_index;
            document.getElementById(this.sliderName).style.zIndex = this.slider_z_index;
        });
    }

}
