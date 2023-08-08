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

    @Output() closePage: EventEmitter<any> = new EventEmitter<any>();
    @Input() entityDetails: EntityDetails = new EntityDetails();
    @Input() travelDisclosure: TravelDisclosure = new TravelDisclosure();
    @Input() isEditMode: any = null;

    conflictHistory = [];
    $subscriptions: Subscription[] = [];
    conflictLookUpList: any = [];
    travelConflictValidationMap = new Map();
    conflictStatus: any = null;
    comment: any = null;
    coiTravelConflictStatusType = null;
    isReadMore: boolean[] = [];
    isShowMore = false;
    travelConflictRO: TravelConflictRO = new TravelConflictRO();

    helpText = [
        'Modify the disclosure status of this travel - entity relation from the Disclosure Status field.',
        'Provide an adequate reason for your decision in the description field provided.'
    ];

    constructor( private _commonService: CommonService,
                 private _service: TravelDisclosureService,
                 private _dataStore: TravelDataStoreService ) { }

    ngOnInit() {
        this.getTravelConflictStatusType();
        this.loadTravelConflictHistory();
        setTimeout(() => {
            openSlider('travel-conflict-slider');
        });
    }

    openConformationModal() {
        document.getElementById('travel-conflict-confirmation-modal-trigger-btn').click();
    }

    validateSliderClose() {
        (this.conflictStatus || this.comment) ? this.openConformationModal() : this.closeConflictSlider();
    }

    closeConflictSlider() {
        closeSlider('travel-conflict-slider');
        setTimeout(() => {
            this.closePage.emit();
        }, 500);
    }

    leavePageClicked(event: boolean) {
        if (event) {
            setTimeout(() => {
                this.closeConflictSlider();
            }, 100);
        }
    }

    private getTravelConflictStatusType(): void {
        this.$subscriptions.push(this._service.getTravelConflictStatusType().subscribe((res: any) => {
            this.conflictLookUpList = res;
        }));
    }

    getTravelConflictHistoryRO(): TravelConflictRO {
        this.travelConflictRO.personId = this.travelDisclosure.personId;
        this.travelConflictRO.travelDisclosureId = this.travelDisclosure.travelDisclosureId;
        this.travelConflictRO.disclosureStatusCode = this.conflictStatus;
        this.travelConflictRO.description = this.comment;
        return this.travelConflictRO;
    }

    manageTravelConflict() {
        if (this.travelConflictValidation()) {
            this.$subscriptions.push(
                this._service.manageTravelConflict(this.getTravelConflictHistoryRO()).subscribe((data: any) => {
                    this.conflictHistory = data;
                    this.isReadMore = [];
                    // this.travelDisclosure.disclosureStatusCode = this.coiTravelConflictStatusType;
                    // this.travelDisclosure.conflictDescription = this.comment;
                    this._dataStore.manualDataUpdate(this.travelDisclosure);
                    this.clearConflictModal();
                    this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Conflict updated successfully.');
                }, _err => {
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in updating disclosure status. Please try again.');
                }));
        }
    }

    travelConflictValidation() {
        this.travelConflictValidationMap.clear();
        if (this.conflictStatus === 'null' || !this.conflictStatus) {
            this.travelConflictValidationMap.set('coiConflictStatusCode', 'Please select conflict status.');
        }
        if (!this.comment) {
            this.travelConflictValidationMap.set('comment', 'Please add a reason.');
        }
        return this.travelConflictValidationMap.size === 0 ? true : false;
    }

    clearConflictModal() {
        this.travelConflictValidationMap.clear();
        this.conflictStatus = null;
        this.comment = null;
        this.coiTravelConflictStatusType = null;
    }

    setCoiTravelConflictStatusType() {
        this.coiTravelConflictStatusType = this.conflictLookUpList.find(status => status.travelConflictStatusCode === this.conflictStatus);
    }

    loadTravelConflictHistory() {
        this.$subscriptions.push(
            this._service.loadTravelConflictHistory(this.travelDisclosure.travelDisclosureId).subscribe((data: any) => {
                this.conflictHistory = data;
            }, _err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching conflict status history. Please try again.');
            }));
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }
}
