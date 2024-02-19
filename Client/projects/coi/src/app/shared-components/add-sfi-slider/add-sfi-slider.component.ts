import { Component, ElementRef, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { hideModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { CommonService } from '../../common/services/common.service';
import { ActivityService } from '../../disclosure/activity-track/activity.service';
import { SfiService } from '../../disclosure/sfi/sfi.service';
import { openCoiSlider } from '../../common/utilities/custom-utilities';

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
        this.showSfiNavBar();
    }

    hideSfiNavBar() {       
        setTimeout(() => {
            this.sfiService.isShowSfiNavBar = false;
        },500);
    }

    showSfiNavBar() {
        if(this.sfiService.isShowSfiNavBar) {
            setTimeout(() => {
                openCoiSlider('add-sfi');
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
    }
}
