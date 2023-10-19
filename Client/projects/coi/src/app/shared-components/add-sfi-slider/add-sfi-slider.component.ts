import { Component, ElementRef, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { hideModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { CommonService } from '../../common/services/common.service';
import { ActivityService } from '../../disclosure/activity-track/activity.service';
import { SfiService } from '../../disclosure/sfi/sfi.service';

@Component({
    selector: 'app-add-sfi-slider',
    templateUrl: './add-sfi-slider.component.html',
    styleUrls: ['./add-sfi-slider.component.scss'],
    providers: [ActivityService]
})
export class AddSfiSliderComponent implements OnInit {

    @Input() disclosureDetails: { disclosureId: any, disclosureNumber: any } = { disclosureId: null, disclosureNumber: null };
    @ViewChild('sfiNavOverlay', { static: true }) sfiNavOverlay: ElementRef;
    scrollHeight: number;
    @Input() coiEntityManageId: any = null;
    @Input() isEditEntity = false;
    @Input() modifyType = '';
    @Output() updatedDataStore = new EventEmitter<number>();
    @Input() revisionReason = '';

    constructor(public sfiService: SfiService, public _commonService: CommonService, private _router: Router) { }

    ngOnInit(): void {
        document.getElementById('COI_SCROLL').classList.add('overflow-hidden');
        this.showSfiNavBar();
    }

    hideSfiNavBar() {
        let slider = document.querySelector('.slider-base');
        slider.classList.remove('slider-opened');        
        setTimeout(() => {
            this.sfiService.isShowSfiNavBar = false;
        },500);
    }

    addBodyScroll() {
        setTimeout(() => {
          document.getElementById('COI_SCROLL').classList.remove('overflow-hidden');
          document.getElementById('COI_SCROLL').classList.add('overflow-y-scroll');
        }, 500);
      }

    showSfiNavBar() {
        if(this.sfiService.isShowSfiNavBar) {
            setTimeout(() => {
                const slider = document.querySelector('.slider-base');
                slider.classList.add('slider-opened');
            });
        }
    }

    emitEvent(event) {
        if (this.isEditEntity) {
            this.updatedDataStore.emit(event);
        }
    }

    addEntityToggle(event) {
        hideModal(event);
    }

    ngOnDestroy() {
        this.addBodyScroll();
    }
}
