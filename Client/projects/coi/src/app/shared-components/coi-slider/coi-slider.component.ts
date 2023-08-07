import { Component, EventEmitter, Input, Output } from '@angular/core';

@Component({
    selector: 'app-coi-slider',
    templateUrl: './coi-slider.component.html',
    styleUrls: ['./coi-slider.component.scss']
})
export class CoiSliderComponent {

    @Input() sliderName = 'coi-slider';
    @Output() closeSlider: EventEmitter<undefined> = new EventEmitter<undefined>();

    emitCloseSlider() {
        this.closeSlider.emit();
    }

}
