import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { TravelDisclosureService } from '../services/travel-disclosure.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { EntityDetails, TravelConflictRO, TravelDisclosure } from '../travel-disclosure-interface';
import { TravelDataStoreService } from '../services/travel-data-store.service';
import { closeSlider, openSlider } from '../../common/utilities/custom-utilities';

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
    conflictLookUpList: any = [];
    $subscriptions: Subscription[] = [];
    travelConflictValidationMap = new Map();
    travelConflictRO: TravelConflictRO = new TravelConflictRO();

    helpText = [
        'Modify the disclosure status of this travel - entity relation from the Disclosure Status field.',
        'Provide an adequate reason for your decision in the description field provided.'
    ];

    constructor( private _commonService: CommonService,
                 private _service: TravelDisclosureService,
                 private _dataStore: TravelDataStoreService ) { }

    ngOnInit(): void {
        this.getTravelConflictStatusType();
        this.loadTravelConflictHistory();
        setTimeout(() => {
            openSlider('travel-conflict-slider');
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
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching conflict status history. Please try again.');
            }));
    }

    private openConformationModal(): void {
        document.getElementById('travel-conflict-confirmation-modal-trigger-btn').click();
    }

    private closeConflictSlider(): void {
        closeSlider('travel-conflict-slider');
        setTimeout(() => {
            this.closePage.emit();
        }, 500);
    }

    private travelConflictValidation(): boolean {
        this.travelConflictValidationMap.clear();
        if (this.disclosureStatusCode === 'null' || !this.disclosureStatusCode) {
            this.travelConflictValidationMap.set('disclosureStatusCode', 'Please select conflict status.');
        }
        if (!this.comment) {
            this.travelConflictValidationMap.set('comment', 'Please add a reason.');
        }
        return this.travelConflictValidationMap.size === 0 ? true : false;
    }

    private getTravelConflictStatusType(): void {
        this.$subscriptions.push(this._service.getTravelConflictStatusType().subscribe((res: any) => {
            this.conflictLookUpList = res;
        }));
    }

    validateSliderClose(): void {
        (this.disclosureStatusCode || this.comment) ? this.openConformationModal() : this.closeConflictSlider();
    }

    setCoiTravelConflictStatusType(): void {
        const disclosureStatusDetails = this.conflictLookUpList.find( status => {
            return status.travelDisclosureStatusCode === this.disclosureStatusCode;
        });
        this.disclosureStatus = disclosureStatusDetails.description;
    }

    leavePageClicked(event: boolean): void {
        if (event) {
            setTimeout(() => {
                this.closeConflictSlider();
            }, 100);
        }
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
                    this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Disclosure Status updated successfully.');
                }, _err => {
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in updating disclosure status. Please try again.');
                }));
        }
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }
}
