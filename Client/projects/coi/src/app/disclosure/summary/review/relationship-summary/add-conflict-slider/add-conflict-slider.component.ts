import { Component, ElementRef, EventEmitter, Input, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { CoiSummaryEventsAndStoreService } from '../../../coi-summary-events-and-store.service';
import { Subscription } from 'rxjs';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../../app-constants';
import { CommonService } from '../../../../../common/services/common.service';
import { openModal } from '../../../../../../../../fibi/src/app/common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../../../../../../fibi/src/app/common/utilities/subscription-handler';

@Component({
    selector: 'app-add-conflict-slider',
    templateUrl: './add-conflict-slider.component.html',
    styleUrls: ['./add-conflict-slider.component.scss']
})
export class AddConflictSliderComponent implements OnInit, OnDestroy {

    @Input() isOpenSlider = true;
    @Output() closePage: EventEmitter<any> = new EventEmitter<any>();
    @Input() entityDetails: any = null;
    @Input() isEditMode: any = null;
    @Input() disclosureMetaData: any = null;
    @ViewChild('addCommentOverlay', { static: true }) addCommentOverlay: ElementRef;

    conflictHistory = [];
    $subscriptions: Subscription[] = [];
    conflictLookUpList: any = [];
    projectConflictValidationMap = new Map();
    conflictStatus: any = null;
    comment: any = null;
    coiProjConflictStatusType = null;
    isReadMore: boolean[] = [];
    isShowMore = false;
    coiConflictStatusType: any = null;

    constructor( public dataStoreService: CoiSummaryEventsAndStoreService,
                 private _commonService: CommonService ) { }

    ngOnInit() {
        document.getElementById('COI_SCROLL').classList.add('overflow-hidden');
        this.showConflictNavBar();
        this.getConflictStatusLookup();
        this.loadProjectConflictHistory();
    }

    showConflictNavBar() {
        if (this.isOpenSlider) {
            setTimeout(() => {
                const slider = document.querySelector('.slider-base');
                slider.classList.add('slider-opened');
            });
        }
    }

    addBodyScroll() {
          document.getElementById('COI_SCROLL').classList.remove('overflow-hidden');
          document.getElementById('COI_SCROLL').classList.add('overflow-y-scroll');
    }    

    validateSliderClose() {
        (this.conflictStatus || this.comment) ? openModal('conflictConfirmationModal') : this.hideConflictNavBar();
    }

    hideConflictNavBar() {
        let slider = document.querySelector('.slider-base');
        slider.classList.remove('slider-opened');
        setTimeout(() => {
            this.isOpenSlider = false;
            this.closePage.emit(this.coiConflictStatusType);
        },500);
    }

    private getConflictStatusLookup(): void {
        this.$subscriptions.push(this.dataStoreService.getProjConflictStatusType().subscribe((res: any) => {
            this.conflictLookUpList = res;
        }));
    }

    updateProjectRelationship() {
        if (this.projectConflictValidation()) {
            this.$subscriptions.push(
                this.dataStoreService.updateProjectRelationship({
                    disclosureDetailsId: this.entityDetails.disclosureDetailsId,
                    documentOwnerPersonId: this.entityDetails.personId,
                    disclosureId: this.entityDetails.disclosureId,
                    conflictStatusCode: this.conflictStatus,
                    comment: this.comment
                }).subscribe((data: any) => {
                    this.conflictHistory = data.coiConflictHistoryList;
                    this.coiConflictStatusType = data.coiConflictStatusTypeDto;
                    this.entityDetails.coiProjConflictStatusType = this.coiProjConflictStatusType;
                    this.entityDetails.comment = this.comment;
                    this.clearConflictModal();
                    this.loadProjectConflictHistory();
                    this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Conflict updated successfully.');
                }, _err => {
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in updating conflict status. Please try again.');
                }));
        }
    }

    projectConflictValidation() {
        this.projectConflictValidationMap.clear();
        if (this.conflictStatus === 'null' || !this.conflictStatus) {
            this.projectConflictValidationMap.set('coiConflictStatusCode', 'Please select conflict status.');
        }
        if (!this.comment) {
            this.projectConflictValidationMap.set('comment', 'Please add a reason.');
        }
        return this.projectConflictValidationMap.size === 0 ? true : false;
    }

    clearConflictModal() {
        this.projectConflictValidationMap.clear();
        this.conflictStatus = null;
        this.comment = null;
        this.coiProjConflictStatusType = null;
    }

    setCoiProjConflictStatusType() {
        this.coiProjConflictStatusType = this.conflictLookUpList.find(status => status.projectConflictStatusCode == this.conflictStatus);
    }

    loadProjectConflictHistory() {
        this.$subscriptions.push(
            this.dataStoreService.loadProjectConflictHistory(this.entityDetails.disclosureDetailsId).subscribe((data: any) => {
                this.conflictHistory = data;
                this.isReadMore = [];
            }, _err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching conflict status history. Please try again.');
            }));
    }

    ngOnDestroy(): void {
        this.addBodyScroll();
        subscriptionHandler(this.$subscriptions);
    }
}
