import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { environment } from 'projects/admin-dashboard/src/environments/environment';
import { DATE_PLACEHOLDER, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from 'projects/fibi/src/app/app-constants';
import { getEndPointOptionsForEntity, getEndPointOptionsForCountry } from 'projects/fibi/src/app/common/services/end-point.config';
import { parseDateWithoutTimestamp, getTotalNoOfDays, compareDates } from 'projects/fibi/src/app/common/utilities/date-utilities';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { TravelDisclosureService } from '../services/travel-disclosure.service';
import { CoiTravelDisclosure, EndpointOptions, TravelCreateModalDetails, TravelDisclosureResponseObject, TravelDisclosureTraveller } from '../travel-disclosure-interface';
import { CommonService } from '../../common/services/common.service';
import { convertToValidAmount } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { TravelDataStoreService } from '../services/travel-data-store.service';

declare var $: any;
@Component({
    selector: 'app-travel-disclosure-form',
    templateUrl: './travel-disclosure-form.component.html',
    styleUrls: ['./travel-disclosure-form.component.scss']
})
export class TravelDisclosureFormComponent implements OnInit, OnDestroy {

    deployMap: string = environment.deployUrl;
    datePlaceHolder: string = DATE_PLACEHOLDER;
    entitySearchOptions: EndpointOptions;
    countrySearchOptions: EndpointOptions;
    $subscriptions: Subscription[] = [];
    clearField = new String('true');
    countryClearField = new String('true');
    entityName = '';
    mandatoryList = new Map();
    dateValidationList = new Map();
    travelDisclosureRO = new CoiTravelDisclosure();
    travellerTypeLookup: Array<TravelDisclosureTraveller>;
    travelStatusTypeLookup: Array<TravelDisclosureTraveller>;
    destination: 'Domestic' | 'International' = 'Domestic';

    constructor(public commonService: CommonService,
                private _router: Router,
                private _service: TravelDisclosureService,
                private _dataStore: TravelDataStoreService) {
        window.scrollTo(0, 0);
    }

    ngOnInit(): void {
        this.entitySearchOptions = getEndPointOptionsForEntity(this.commonService.baseUrl);
        this.countrySearchOptions = getEndPointOptionsForCountry(this.commonService.fibiUrl);
        this.loadDisclosureDetails();
        this.listenDataChangeFromStore();
        this.loadTravellerTypesLookup();
        this.loadTravelStatusTypesLookup();
        this.handleTravelDisclosureSubmission();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private loadDisclosureDetails(): void {
        if (this._dataStore.getData().travelDisclosureId) {
            this.setDisclosureDetails(this._dataStore.getData());
        }
    }

    private setDisclosureDetails(responseObject: TravelDisclosureResponseObject): void {
        this.entitySearchOptions.defaultValue = responseObject.travelEntityName;
        this.entityName = responseObject.travelEntityName;
        this.travelDisclosureRO.entityId = responseObject.entityId;
        this.travelDisclosureRO.entityNumber = responseObject.entityNumber;
        this.travelDisclosureRO.travellerTypeCode = responseObject.travellerTypeCodeList;
        this.travelDisclosureRO.travelTitle = responseObject.travelTitle;
        this.travelDisclosureRO.isInternationalTravel = responseObject.isInterNationalTravel;
        this.travelDisclosureRO.travelState = responseObject.travelState;
        this.travelDisclosureRO.destinationCountry = responseObject.destinationCountry;
        this.countrySearchOptions.defaultValue = responseObject.destinationCountry;
        this.destination = this.travelDisclosureRO.destinationCountry ? 'International' : 'Domestic';
        this.travelDisclosureRO.destinationCity = responseObject.destinationCity;
        this.travelDisclosureRO.purposeOfTheTrip = responseObject.purposeOfTheTrip;
        this.travelDisclosureRO.relationshipToYourResearch = responseObject.relationshipToYourResearch;
        this.travelDisclosureRO.travelAmount = !responseObject.travelAmount ? null : convertToValidAmount(responseObject.travelAmount);
        this.travelDisclosureRO.travelStartDate = parseDateWithoutTimestamp(responseObject.travelStartDate);
        this.travelDisclosureRO.travelEndDate = parseDateWithoutTimestamp(responseObject.travelEndDate);
        this.travelDisclosureRO.travelDisclosureId = responseObject.travelDisclosureId;
        this.travelDisclosureRO.homeUnit = responseObject.homeUnitNumber;
        this.travelDisclosureRO.description = responseObject.description;
        this.travelDisclosureRO.personId = responseObject.personId;
    }

    private handleTravelDisclosureSubmission(): void {
        this.$subscriptions.push(this._service.saveOrCopySubject.subscribe((event: string) => {
            switch (event) {
                case 'CERTIFY_DISCLOSURE':
                    this.saveTravelDisclosure();
                    break;
                case 'CERTIFY_COPY_DISCLOSURE':
                    this.saveTravelDisclosure();
                    break;
                case 'SAVE_DISCLOSURE':
                    this.saveTravelDisclosure();
                    break;
                default:
                    break;
            }
        }));
    }

    private loadTravelStatusTypesLookup(): void {
        this.$subscriptions.push(this._service.loadTravelStatusTypesLookup()
            .subscribe((data: any) => {
                if (data) {
                    this.travelStatusTypeLookup = data;
                }
            }));
    }

    private loadTravellerTypesLookup(): void {
        this.$subscriptions.push(this._service.loadTravellerTypesLookup()
            .subscribe((data: any) => {
                if (data) {
                    this.travellerTypeLookup = data;
                    this.setCheckBoxValue();
                }
            }));
    }

    private setCheckBoxValue(): void {
        if (this.travelDisclosureRO.travellerTypeCode.length > 0) {
            for (const type of this.travelDisclosureRO.travellerTypeCode) {
                const matchingDetail = this.travellerTypeLookup.find(details => details.travelerTypeCode === type);
                if (matchingDetail) {
                    matchingDetail.isChecked = true;
                }
            }
        }
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.loadDisclosureDetails();
            })
        );
    }

    private getTravelCreateModalDetails(): void {
        if (!this._dataStore.getData().travelDisclosureId) {
            const travelCreateModalDetails: TravelCreateModalDetails = this._dataStore.getCreateModalDetails();
            this.travelDisclosureRO.homeUnit = travelCreateModalDetails.homeUnit;
            this.travelDisclosureRO.description = travelCreateModalDetails.description;
            this.travelDisclosureRO.personId = travelCreateModalDetails.personId;
        }
    }

    private getAllTravelDisclosureValues(requestObject: CoiTravelDisclosure): CoiTravelDisclosure {
        this.getTravelCreateModalDetails();
        this.getTravellerTypeCode();
        this.setValuesForDestinationType();
        requestObject.isSponsoredTravel = true;
        requestObject.travelAmount = !requestObject.travelAmount ? null : convertToValidAmount(requestObject.travelAmount);
        requestObject.travelStartDate = parseDateWithoutTimestamp(requestObject.travelStartDate);
        requestObject.travelEndDate = parseDateWithoutTimestamp(requestObject.travelEndDate);
        requestObject.noOfDays = getTotalNoOfDays(requestObject.travelStartDate, requestObject.travelEndDate);
        requestObject.versionNumber = '1';
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
        if (this.destination === 'Domestic') {
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
        this.travelDisclosureRO.entityId = event && event.entityId || null;
        this.travelDisclosureRO.entityNumber = event && event.entityNumber || null;
    }

    selectTravelCountry(event: any): void {
        this.travelDisclosureRO.destinationCountry = event && event.countryName || null;
    }

    getTravellerTypeCode(): void {
        this.travelDisclosureRO.travellerTypeCode = [];
        for (const details of this.travellerTypeLookup) {
            if (details.isChecked === true) {
                this.travelDisclosureRO.travellerTypeCode.push(details.travelerTypeCode);
            }
        }
    }

    setValuesForDestinationType(): void {
        this.setUnSavedChanges();
        if (this.destination === 'Domestic') {
            this.travelDisclosureRO.destinationCountry = '';
            this.travelDisclosureRO.isInternationalTravel = false;
            this.countrySearchOptions.defaultValue = '';
        } else {
            this.travelDisclosureRO.travelState = '';
            this.travelDisclosureRO.isInternationalTravel = true;
        }
    }

    setUnSavedChanges(): void {
        this._service.setUnSavedChanges(true, 'Travel Details');
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

    private saveTravelDisclosure(): void {
        this.getAllTravelDisclosureValues(this.travelDisclosureRO);
        if (this.validateForm()) {
            this.$subscriptions.push(this._service.createCoiTravelDisclosure(this.travelDisclosureRO)
                .subscribe((res: any) => {
                    if (res) {
                        this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Travel Disclosure Saved Successfully');
                        this._service.setUnSavedChanges(false, '');
                        this.loadTravelDisclosure(res.travelDisclosureId);
                    }
                }, (err) => {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in Saving Travel Disclosure');
                })
            );
        }
    }

    private loadTravelDisclosure(travelDisclosureId: string): void {
        this._service.loadTravelDisclosure(travelDisclosureId)
        .subscribe((responseObject: TravelDisclosureResponseObject) => {
            if (responseObject) {
                this._dataStore.removeCreateModalDetails();
                this._dataStore.manualDataUpdate(responseObject);
                this._router.navigate([], {
                    queryParams: {
                        disclosureId: responseObject.travelDisclosureId
                    },
                    queryParamsHandling: 'merge',
                });
            }
        });
    }
}


