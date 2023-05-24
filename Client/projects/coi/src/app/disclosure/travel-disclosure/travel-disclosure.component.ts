import { Component, OnInit, OnDestroy, Input } from '@angular/core';
import { Subscription } from 'rxjs';
import { getEndPointOptionsForEntity, getEndPointOptionsForCountry } from '../../../../../fibi/src/app/common/services/end-point.config';
import { CoiService } from '../services/coi.service';
import { environment } from '../../../environments/environment';
import { CommonService } from '../../common/services/common.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { compareDates, parseDateWithoutTimestamp, getTotalNoOfDays } from '../../../../../fibi/src/app/common/utilities/date-utilities';
import { CoiTravelDisclosure, TravelDisclosureTraveller } from '../coi-interface';
import { DATE_PLACEHOLDER, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from 'projects/fibi/src/app/app-constants';
import { slideHorizontal } from 'projects/fibi/src/app/common/utilities/animations';
import { UserDashboardService } from '../../user-dashboard/user-dashboard.service';
declare var $: any;
@Component({
    selector: 'app-travel-disclosure',
    templateUrl: './travel-disclosure.component.html',
    styleUrls: ['./travel-disclosure.component.scss'],
    animations: [slideHorizontal]
})
export class TravelDisclosureComponent implements OnInit, OnDestroy {
    deployMap = environment.deployUrl;
    datePlaceHolder = DATE_PLACEHOLDER;
    EntitySearchOptions: any;
    countrySearchOptions: any;
    $subscriptions: Subscription[] = [];
    clearField: String;
    countryClearField: String;
    entityName: String;
    mandatoryList = new Map();
    dateValidationList = new Map();
    isCopyTravelDisclosure = false;
    @Input() travelDisclosureRO = new CoiTravelDisclosure();
    travellerType: Array<TravelDisclosureTraveller>;
    travelStatusType: Array<TravelDisclosureTraveller>;
    destination: 'Domestic' | 'International' = 'Domestic';
    travelStatusDescription: 'Pending' | 'Submitted' | 'Approved' = 'Pending';

    constructor(public coiService: CoiService, public commonService: CommonService,
        public userDashboardService: UserDashboardService) { }

    ngOnInit(): void {
        this.EntitySearchOptions = getEndPointOptionsForEntity(this.commonService.baseUrl);
        this.countrySearchOptions = getEndPointOptionsForCountry(this.commonService.fibiUrl);
        this.loadTravellerTypesLookup();
        this.loadTravelStatusTypesLookup();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    loadTravelStatusTypesLookup(): void {
        this.$subscriptions.push(this.coiService.loadTravelStatusTypesLookup()
            .subscribe((data: any) => {
                if (data) {
                    this.travelStatusType = data;
                }
            }));
    }

    loadTravellerTypesLookup(): void {
        this.$subscriptions.push(this.coiService.loadTravellerTypesLookup()
            .subscribe((data: any) => {
                if (data) {
                    this.travellerType = data;
                }
            }));
    }

    setCheckBoxValue(): void {
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

    getTravelStatusCode(): void {
        for (let details of this.travelStatusType) {
            if (details.description == this.travelStatusDescription) {
                this.travelDisclosureRO.travelStatusCode = details.travelStatusCode;
            }
        }
    }

    setValuesForDestinationType(): void {
        if (this.destination == 'Domestic') {
            this.travelDisclosureRO.destinationCountry = '';
            this.travelDisclosureRO.isInternationalTravel = false;
        }
        else {
            this.travelDisclosureRO.travelState = '';
            this.travelDisclosureRO.isInternationalTravel = true;
        }
    }

    getAllTravelDisclosureValues(requestObject: any): any {
        this.getTravellerTypeCode();
        this.getTravelStatusCode();
        this.setValuesForDestinationType();
        requestObject.isSponsoredTravel = true;
        requestObject.personId= this.commonService.getCurrentUserDetail('personId');
        requestObject.travelAmount = (Number(requestObject.travelAmount)) ? Number(requestObject.travelAmount) : null;
        requestObject.travelStartDate = parseDateWithoutTimestamp(requestObject.travelStartDate);
        requestObject.travelEndDate = parseDateWithoutTimestamp(requestObject.travelEndDate);
        requestObject.noOfDays = getTotalNoOfDays(requestObject.travelStartDate, requestObject.travelEndDate);
        requestObject.noOfDays = getTotalNoOfDays(requestObject.travelStartDate, requestObject.travelEndDate);
        return requestObject;
    }

    validateForm(): boolean {
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
        }
        else if (!this.travelDisclosureRO.destinationCountry) {
            this.mandatoryList.set('country', 'Please enter the country.');
        }
        this.validateDates();
        return this.mandatoryList.size !== 0 || !this.validateDates() ? false : true;
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
        }
        else {
            this.dateValidationList.set('endDate', 'Please select the end date.');
        }
        return this.dateValidationList.size ? false : true;
    }

    clearDisclosureModal(): void {
        this.userDashboardService.isShowTravelDisclosure = false;
        this.EntitySearchOptions = getEndPointOptionsForEntity(this.commonService.baseUrl);
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
            this.$subscriptions.push(this.coiService.createCoiTravelDisclosure(this.travelDisclosureRO)
                .subscribe((res: any) => {
                    if (res) {
                        this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Travel Disclosure Saved Successfully');
                    }
                }, (err) => {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in Saving Travel Disclosure');
                }))
            if (this.isCopyTravelDisclosure == true) {
                this.isCopyTravelDisclosure = false;
                // add your code here for the copy function/api call
            }
            else {
                this.clearDisclosureModal();
            }
            $('#createTravelDisclosureModal').modal('hide');
        }
        else {
            // for validation error message
        }
    }

    certifyCopyTravelDisclosure(): void {
        this.isCopyTravelDisclosure = true;
        this.certifyTravelDisclosure();
    }

}

