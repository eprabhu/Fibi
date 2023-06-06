import { Component, ElementRef, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { slideHorizontalFast } from '../../../../../fibi/src/app/common/utilities/animations';
import { hideModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { CommonService } from '../../common/services/common.service';
import { ActivityService } from '../../disclosure/activity-track/activity.service';
import { SfiService } from '../../disclosure/sfi/sfi.service';

@Component({
    selector: 'app-add-sfi-slider',
    templateUrl: './add-sfi-slider.component.html',
    styleUrls: ['./add-sfi-slider.component.scss'],
    providers: [ActivityService],
    animations: [slideHorizontalFast]
})
export class AddSfiSliderComponent implements OnInit {

    @Input() disclosureDetails: { disclosureId: any, disclosureNumber: any } = { disclosureId: null, disclosureNumber: null };

    scrollHeight: number;
    @ViewChild('sfiNavOverlay', { static: true }) sfiNavOverlay: ElementRef;
    @Input() coiEntityManageId: any = null;
    @Input() isEditEntity = false;
    @Input() changeType = '';
    @Output() updatedDataStore = new EventEmitter<boolean>();

    constructor(public sfiService: SfiService, public _commonService: CommonService, private _router: Router) { }

    ngOnInit(): void {
        this.showSfiNavBar();
    }

    hideSfiNavBar() {
        this.sfiService.isShowSfiNavBar = false;
        this.showSfiNavBar();
    }

    showSfiNavBar() {
        if (this.sfiService.isShowSfiNavBar) {
            this.scrollHeight = document.documentElement.scrollTop;
            document.body.style.overflow = 'hidden';
            document.documentElement.style.top = - this.scrollHeight + 'px';
        } else {
            document.body.style.overflow = 'auto';
            document.documentElement.scrollTop = this.scrollHeight;
        }
    }

    emitEvent() {
        if (this.isEditEntity) {
            this.updatedDataStore.emit(true);
        }
    }

    addEntityToggle(event) {
        hideModal(event);
    }

}
