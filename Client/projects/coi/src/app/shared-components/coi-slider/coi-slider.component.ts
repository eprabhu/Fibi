import { Component, ElementRef, EventEmitter, HostListener, Input, OnInit, Output, ViewChild } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { closeCoiSlider } from '../../common/utilities/custom-utilities';
import { openModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
@Component({
    selector: 'app-coi-slider',
    templateUrl: './coi-slider.component.html',
    styleUrls: ['./coi-slider.component.scss']
})
export class CoiSliderComponent implements OnInit {

    isEnableSlider = false;

    @Input() isHeaderNeeded = false;
    @Input() isStickyNeeded = true;
    @Input() slider_z_index: any = null;
    @Input() overlay_z_index: any = null;
    @Input() isChangedFieldValue = false;
    @Output() closeSlider: EventEmitter<undefined> = new EventEmitter<undefined>();
    @Input() elementId = 'coiSlider';
    @Input() sliderWidth: 'w-25' | 'w-50' | 'w-75' | 'w-100' | 'w-auto' = 'w-75';
    @ViewChild('SliderParentElement', { static: true }) sliderParentElement: ElementRef;
    @ViewChild('Backdrop', { static: false }) backdrop: ElementRef;

    constructor(private _elementRef: ElementRef, private _commonService: CommonService) { }

    ngOnInit() {
        setTimeout(() => {
            if (this.slider_z_index && this.overlay_z_index) {
                this.backdrop.nativeElement.style.zIndex = this.overlay_z_index;
                this.sliderParentElement.nativeElement.style.zIndex = this.slider_z_index;
            }
            this.sliderParentElement.nativeElement.classList.add(this.sliderWidth);
            this.backdrop.nativeElement.classList.add('show');
            this.isEnableSlider = true;
        });
    }

    closeSliderWindow() {
        !this.isChangedFieldValue ?  this.emitCloseSlider() : openModal(`${this.elementId}-confirmation-modal`);
    }

    emitCloseSlider() {
        this.isEnableSlider = false;
        this.closeSlider.emit();
        closeCoiSlider(this.elementId);
        this.backdrop.nativeElement.classList.remove('show');
    }

    @HostListener('document:keydown.escape', ['$event'])
    handleEscapeEvent(event: any): void {
        if ((event.key === 'Escape' || event.key === 'Esc') && this.isEnableSlider) {
            this.closeSliderWindow();
        }
    }

    leaveSlider() {  
        this.emitCloseSlider();
    }

    
}
