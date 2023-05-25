import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { environment } from 'projects/admin-dashboard/src/environments/environment';
import { DATE_PLACEHOLDER, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from 'projects/fibi/src/app/app-constants';
import { getEndPointOptionsForEntity, getEndPointOptionsForCountry } from 'projects/fibi/src/app/common/services/end-point.config';
import { parseDateWithoutTimestamp, getTotalNoOfDays, compareDates } from 'projects/fibi/src/app/common/utilities/date-utilities';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { TravelDisclosureService } from '../travel-disclosure.service';
import { CoiTravelDisclosure, TravelDisclosureTraveller } from '../travel-disclosure-interface';
import { CommonService } from '../../common/services/common.service';
import { CoiService } from '../../disclosure/services/coi.service';
import { DataStoreService } from '../../disclosure/services/data-store.service';

declare var $: any;
@Component({
    selector: 'app-travel-disclosure-form',
    templateUrl: './travel-disclosure-form.component.html',
    styleUrls: ['./travel-disclosure-form.component.scss']
})
export class TravelDisclosureFormComponent implements OnInit, OnDestroy {

    deployMap: string = environment.deployUrl;
    datePlaceHolder: string = DATE_PLACEHOLDER;
    entitySearchOptions: any;
    countrySearchOptions: any;
    $subscriptions: Subscription[] = [];
    clearField: String;
    countryClearField: String;
    entityName: String;
    mandatoryList = new Map();
    dateValidationList = new Map();
    isCopyTravelDisclosure: boolean = false;
    travelDisclosureRO = new CoiTravelDisclosure();
    travellerType: Array<TravelDisclosureTraveller>;
    travelStatusType: Array<TravelDisclosureTraveller>;
    destination: 'Domestic' | 'International' = 'Domestic';
    travelStatusDescription: 'Pending' | 'Submitted' | 'Approved' = 'Pending';

    constructor(public coiService: CoiService,
        public commonService: CommonService,
        public router: Router,
        public service: TravelDisclosureService,
        private _dataStore: DataStoreService) { }

    ngOnInit(): void {
        this.entitySearchOptions = getEndPointOptionsForEntity(this.commonService.baseUrl);
        this.countrySearchOptions = getEndPointOptionsForCountry(this.commonService.fibiUrl);
        this.loadTravellerTypesLookup();
        this.loadTravelStatusTypesLookup();
        this.handleTravelDisclosureSubmission();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private handleTravelDisclosureSubmission(): void {
        this.service.travelDisclosureSubject.subscribe((event) => {
            if (event == 'certify') {
                this.certifyTravelDisclosure();
            } else if (event == 'certifycopy') {
                this.certifyCopyTravelDisclosure();
            }
        });
    }
    private loadTravelStatusTypesLookup(): void {
        this.$subscriptions.push(this.service.loadTravelStatusTypesLookup()
            .subscribe((data: any) => {
                if (data) {
                    this.travelStatusType = data;
                }
            }));
    }

    private loadTravellerTypesLookup(): void {
        this.$subscriptions.push(this.service.loadTravellerTypesLookup()
            .subscribe((data: any) => {
                if (data) {
                    this.travellerType = data;
                }
            }));
    }

    private setCheckBoxValue(): void {
        for (let details of this.travellerType) {
            details.isChecked = false;
        }
        if (this.travelDisclosureRO.travellerTypeCode.length > 0) {
            for (let type of this.travelDisclosureRO.travellerTypeCode) {
                for (let details of this.travellerType) {
                    if (type == details.travelerTypeCode) {
                        details.isChecked = true;
                    }
                }
            }
        }
    }

    private getTravelStatusCode(): void {
        for (let details of this.travelStatusType) {
            if (details.description == this.travelStatusDescription) {
                this.travelDisclosureRO.travelStatusCode = details.travelStatusCode;
            }
        }
    }

    private getAllTravelDisclosureValues(requestObject: any): any {
        this.getTravellerTypeCode();
        this.getTravelStatusCode();
        this.setValuesForDestinationType();
        requestObject.isSponsoredTravel = true;
        requestObject.personId = this.commonService.getCurrentUserDetail('personId');
        requestObject.travelAmount = (Number(requestObject.travelAmount)) ? Number(requestObject.travelAmount) : null;
        requestObject.travelStartDate = parseDateWithoutTimestamp(requestObject.travelStartDate);
        requestObject.travelEndDate = parseDateWithoutTimestamp(requestObject.travelEndDate);
        requestObject.noOfDays = getTotalNoOfDays(requestObject.travelStartDate, requestObject.travelEndDate);
        requestObject.noOfDays = getTotalNoOfDays(requestObject.travelStartDate, requestObject.travelEndDate);
        return requestObject;
    }

    private validateForm(): boolean {
        this.mandatoryList.clear();
        if (!this.travelDisclosureRO.travellerTypeCode.length) {
            this.mandatoryList.set('traveller', 'Please select at least one traveller.');
        }
        if (!this.entityName) {
            this.mandatoryList.set('entity', 'Please enter the entity name.');
        }
        if (!this.travelDisclosureRO.travelTitle) {
            this.mandatoryList.set('title', 'Please enter the trip title.');
        }
        if (!this.travelDisclosureRO.destinationCity) {
            this.mandatoryList.set('city', 'Please enter the city.');
        }
        if (!this.travelDisclosureRO.purposeOfTheTrip) {
            this.mandatoryList.set('purpose', 'Please enter the purpose of the trip.');
        }
        if (!this.travelDisclosureRO.relationshipToYourResearch) {
            this.mandatoryList.set('relationship', 'Please enter the relationship.');
        }
        if (!this.travelDisclosureRO.travelAmount) {
            this.mandatoryList.set('amount', 'Please enter the amount.');
        }
        if (this.destination == 'Domestic') {
            if (!this.travelDisclosureRO.travelState) {
                this.mandatoryList.set('state', 'Please enter the state.');
            }
        } else if (!this.travelDisclosureRO.destinationCountry) {
            this.mandatoryList.set('country', 'Please enter the country.');
        }
        this.validateDates();
        return this.mandatoryList.size !== 0 || !this.validateDates() ? false : true;
    }

    selectedEntityEvent(event: any): void {
        this.entityName = event && event.entityName || null;
        this.travelDisclosureRO.versionNumber = event && event.versionNumber || null;
        this.travelDisclosureRO.entityId = event && event.entityId || null;
        this.travelDisclosureRO.entityNumber = event && event.entityNumber || null;
    }

    selectTravelCountry(event: any): void {
        this.travelDisclosureRO.destinationCountry = event && event.countryName || null;
    }

    getTravellerTypeCode(): void {
        this.travelDisclosureRO.travellerTypeCode = [];
        for (let details of this.travellerType) {
            if (details.isChecked == true) {
                this.travelDisclosureRO.travellerTypeCode.push(details.travelerTypeCode);
            }
        }
    }

    setValuesForDestinationType(): void {
        if (this.destination == 'Domestic') {
            this.travelDisclosureRO.destinationCountry = '';
            this.travelDisclosureRO.isInternationalTravel = false;
        } else {
            this.travelDisclosureRO.travelState = '';
            this.travelDisclosureRO.isInternationalTravel = true;
        }
    }

    triggerConfirmationModal(): void {
        this._dataStore.dataChanged = true;
        this.coiService.unSavedModules = 'Travel Details';
    }

    validateDates(): boolean {
        this.dateValidationList.clear();
        if (!this.travelDisclosureRO.travelStartDate) {
            this.dateValidationList.set('startDate', 'Please select the start date.');
        }

        if (this.travelDisclosureRO.travelEndDate) {
            if (!((compareDates(this.travelDisclosureRO.travelStartDate,
                this.travelDisclosureRO.travelEndDate) === 1) ? false : true)) {
                this.dateValidationList.set('endDate', 'Please provide a valid end date.');
            }
        } else {
            this.dateValidationList.set('endDate', 'Please select the end date.');
        }
        return this.dateValidationList.size ? false : true;
    }

    clearDisclosureModal(): void {
        this.entitySearchOptions = getEndPointOptionsForEntity(this.commonService.baseUrl);
        this.countrySearchOptions = getEndPointOptionsForCountry(this.commonService.fibiUrl);
        this.clearField = new String('true');
        this.countryClearField = new String('true');
        this.mandatoryList.clear();
        this.dateValidationList.clear();
        this.travelDisclosureRO = new CoiTravelDisclosure();
        this.setCheckBoxValue();
    }

    certifyTravelDisclosure(): void {
        this.getAllTravelDisclosureValues(this.travelDisclosureRO);
        if (this.validateForm()) {
            this.$subscriptions.push(this.service.createCoiTravelDisclosure(this.travelDisclosureRO)
                .subscribe((res: any) => {
                    if (res) {
                        this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Travel Disclosure Saved Successfully');
                        this.coiService.unSavedModules = '';
                        this._dataStore.dataChanged = false;
                    }
                }, (err) => {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in Saving Travel Disclosure');
                }))
            if (this.isCopyTravelDisclosure == true) {
                this.isCopyTravelDisclosure = false;
            } else {
                this.clearDisclosureModal();
            }
            $('#createTravelDisclosureModal').modal('hide');
        }
    }

    certifyCopyTravelDisclosure(): void {
        this.isCopyTravelDisclosure = true;
        this.certifyTravelDisclosure();
    }
}


