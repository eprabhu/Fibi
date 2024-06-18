import { Component, ElementRef, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { hideModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { CommonService } from '../../common/services/common.service';
import { ActivityService } from '../../disclosure/activity-track/activity.service';
import { SfiService } from '../../disclosure/sfi/sfi.service';
import { openCoiSlider } from '../../common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { HTTP_ERROR_STATUS } from '../../app-constants';

@Component({
    selector: 'app-add-sfi-slider',
    templateUrl: './add-sfi-slider.component.html',
    styleUrls: ['./add-sfi-slider.component.scss'],
    providers: [ActivityService]
})
export class AddSfiSliderComponent implements OnInit {

    private readonly _moduleCode = 'SFI53';

    @Input() disclosureDetails: { disclosureId: any, disclosureNumber: any } = { disclosureId: null, disclosureNumber: null };
    @ViewChild('sfiNavOverlay', { static: true }) sfiNavOverlay: ElementRef;
    scrollHeight: number;
    @Input() coiEntityManageId: any = null;
    @Input() isEditEntity = false;
    @Input() modifyType = '';
    @Output() updatedDataStore = new EventEmitter<number>();
    @Input() revisionReason = '';

    sfiSliderSectionConfig: any;
    isShowAddSfiPage: boolean = false;
    $subscriptions: Subscription[] = [];

    constructor(public sfiService: SfiService, public _commonService: CommonService, private _router: Router) { }

    ngOnInit(): void {
        this.getSfiSectionConfig();    
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
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

    getSfiSectionConfig() : void {
        this.$subscriptions.push(
        this._commonService.getDashboardActiveModules(this._moduleCode).subscribe((data) => {
            this.sfiSliderSectionConfig = data;
            this.openSlider();
        },
            _err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in Fetching Active Modules.');
                this.openSlider();
            }));
    }

    openSlider() : void {
        this.isShowAddSfiPage = true;
        this.showSfiNavBar();
    }
}
