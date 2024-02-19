import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { TravelDisclosureService } from '../services/travel-disclosure.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { EntityDetails, TravelConflictRO, TravelDisclosure } from '../travel-disclosure-interface';
import { TravelDataStoreService } from '../services/travel-data-store.service';
import { closeSlider, openCoiSlider, openCommonModal, openSlider } from '../../common/utilities/custom-utilities';

@Component({
    selector: 'app-travel-add-conflict-slider',
    templateUrl: './travel-add-conflict-slider.component.html',
    styleUrls: ['./travel-add-conflict-slider.component.scss']
})
export class TravelAddConflictSliderComponent implements OnInit, OnDestroy {

    @Input() isEditMode: any = null;
    @Input() entityDetails: EntityDetails = new EntityDetails();
    @Input() travelDisclosure: TravelDisclosure = new TravelDisclosure();
    @Output() closePage: EventEmitter<any> = new EventEmitter<any>();

    isShowMore = false;
    comment: string = null;
    disclosureStatusCode: string = null;
    disclosureStatus = null;
    conflictHistory: any = [];
    isReadMore: boolean[] = [];
    disclosureStatusLookUpList: any = [];
    $subscriptions: Subscription[] = [];
    travelConflictValidationMap = new Map();
    travelConflictRO: TravelConflictRO = new TravelConflictRO();

    helpText = [
        'Modify the disclosure status of this travel - entity relation from the Disclosure Status field.',
        'Provide an adequate reason for your decision in the description field provided.'
    ];

    constructor( public commonService: CommonService,
                 private _service: TravelDisclosureService,
                 private _dataStore: TravelDataStoreService ) { }

    ngOnInit(): void {
        this.getTravelConflictStatusType();
        this.loadTravelConflictHistory();
        setTimeout(() => {
            openCoiSlider('travel-conflict-slider');
        });
    }

    private getTravelConflictHistoryRO(): TravelConflictRO {
        this.travelConflictRO.personId = this.travelDisclosure.personId;
        this.travelConflictRO.travelDisclosureId = this.travelDisclosure.travelDisclosureId;
        this.travelConflictRO.disclosureStatusCode = this.disclosureStatusCode;
        this.travelConflictRO.description = this.comment;
        return this.travelConflictRO;
    }

    private loadTravelConflictHistory(): void {
        this.$subscriptions.push(
            this._service.loadTravelConflictHistory(this.travelDisclosure.travelDisclosureId).subscribe((data: any) => {
                this.conflictHistory = data;
            }, _err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching conflict status history. Please try again.');
            }));
    }

    closeConflictSlider(): void {
        setTimeout(() => {
            this.closePage.emit();
        }, 500);
    }

    private travelConflictValidation(): boolean {
        this.travelConflictValidationMap.clear();

        if (!this.comment) {
            this.travelConflictValidationMap.set('comment', 'Please add a reason.');
        }

        if (this.disclosureStatusCode === 'null' || !this.disclosureStatusCode) {
            this.travelConflictValidationMap.set('disclosureStatusCode', 'Please select disclosure status.');
        }

        if (this.disclosureStatusCode  === this.travelDisclosure.disclosureStatusCode) {
            this.travelConflictValidationMap.set('duplicateDisclosure', 'You are trying to update the disclosure status with the current disclosure status.');
            this.travelConflictValidationMap.delete('disclosureStatusCode');
        }

        return this.travelConflictValidationMap.size === 0;
    }

    private getTravelConflictStatusType(): void {
        this.$subscriptions.push(this._service.getTravelConflictStatusType().subscribe((res: any) => {
            this.disclosureStatusLookUpList = res;
        }));
    }

    setCoiTravelConflictStatusType(): void {
        const disclosureStatusDetails = this.disclosureStatusLookUpList.find( status => {
            return status.disclosureStatusCode === this.disclosureStatusCode;
        });
        this.disclosureStatus = disclosureStatusDetails.description;
    }

    leavePageClicked(): void {
        setTimeout(() => {
            this.closeConflictSlider();
        }, 100);
    }

    clearConflictModal(): void {
        this.travelConflictValidationMap.clear();
        this.disclosureStatusCode = null;
        this.comment = null;
        this.disclosureStatus = null;
    }

    manageTravelConflict(): void {
        if (this.travelConflictValidation()) {
            this.$subscriptions.push(
                this._service.manageTravelConflict(this.getTravelConflictHistoryRO()).subscribe((data: any) => {
                    this.conflictHistory = data;
                    this.isReadMore = [];
                    this.travelDisclosure.disclosureStatusCode = this.disclosureStatusCode;
                    this.travelDisclosure.disclosureStatus = this.disclosureStatus;
                    this._dataStore.manualDataUpdate(this.travelDisclosure);
                    this.clearConflictModal();
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Disclosure Status updated successfully.');
                }, _err => {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in updating disclosure status. Please try again.');
                }));
        }
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    getWarningClass(typeCode): string {
        switch (typeCode) {
            case '1':
                return 'invalid';
            case '2':
                return 'medium-risk';
            case '3':
                return 'low-risk';
            default:
                return;
        }
    }

    isFieldValueChanges(): boolean {
        return !!((this.disclosureStatusCode || this.comment));
    }
}
