import { Component, ElementRef, EventEmitter, Input, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { CoiSummaryEventsAndStoreService } from '../../../coi-summary-events-and-store.service';
import { Subscription } from 'rxjs';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../../app-constants';
import { CommonService } from '../../../../../common/services/common.service';
import { isEmptyObject, openModal } from '../../../../../../../../fibi/src/app/common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { CoiService } from '../../../../services/coi.service';
import { openCoiSlider } from '../../../../../common/utilities/custom-utilities';
import { DataStoreService } from '../../../../services/data-store.service';
import { COI } from '../../../../coi-interface';
import { CoiSummaryService } from '../../../coi-summary.service';

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
    @Input() relationshipType : any[] = [];
    @ViewChild('addCommentOverlay', { static: true }) addCommentOverlay: ElementRef;

    conflictHistory = [];
    $subscriptions: Subscription[] = [];
    conflictLookUpList: any = [];
    projectConflictValidationMap = new Map();
    conflictStatus: any = null;
    comment: any = null;
    coiProjConflictStatusType = null;
    isReadMore: boolean[] = [];
    titleReadMore = false;
    coiConflictStatusType: any = null;
    coiData = new COI();
    relationshipTypeCache = {};

    constructor( public dataStoreService: CoiSummaryEventsAndStoreService,
                 private _commonService: CommonService,  public coiService: CoiService,
                 public dataStore: DataStoreService,public coiSummaryService: CoiSummaryService) { }

    ngOnInit() {
        this.showConflictNavBar();
        this.getConflictStatusLookup();
        this.loadProjectConflictHistory();
        this.coiData = this.dataStore.getData();        
    }

    showConflictNavBar() {
        if (this.isOpenSlider) {
            setTimeout(() => {
               openCoiSlider('add-conflict');
            });
        }
    }

    hideConflictNavBar() {
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
                    if (_err.status === 405) {
                        this.coiService.concurrentUpdateAction = 'Modify Conflict';
                    } else {
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in updating conflict status. Please try again.');
                    }
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
        if (this.conflictStatus == this.entityDetails.coiProjConflictStatusType.projectConflictStatusCode) {
			this.projectConflictValidationMap.set('duplicateStatus', 'You are trying to update the conflict with the current conflict status of the disclosure.');
			this.projectConflictValidationMap.delete('riskLevelCode');
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


    isEmptyHistory(): boolean {
        return isEmptyObject(this.conflictHistory);
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    isFieldValueChanges(): boolean {
       return !!((this.conflictStatus  || this.comment));
    }

    getColorBadges(disclosure): string {
        if (disclosure?.travelDisclosureId) {
            return 'bg-travel-clip';
        }
        switch (disclosure.fcoiTypeCode) {
            case '1':
                return 'bg-fcoi-clip';
            case '2':
                return 'bg-proposal-clip';
            case '3':
                return 'bg-award-clip';
            default:
                return;
        }
    }
}
