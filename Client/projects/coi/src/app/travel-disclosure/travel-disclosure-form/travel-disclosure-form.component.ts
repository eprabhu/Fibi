import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { environment } from '../../../../../admin-dashboard/src/environments/environment';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../../fibi/src/app/app-constants';
import { DATE_PLACEHOLDER } from '../../../../src/app/app-constants';
import { getEndPointOptionsForEntity, getEndPointOptionsForCountry } from '../../../../../fibi/src/app/common/services/end-point.config';
import { parseDateWithoutTimestamp, getTotalNoOfDays, compareDates } from '../../../../../fibi/src/app/common/utilities/date-utilities';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { TravelDisclosureService } from '../services/travel-disclosure.service';
import { CoiTravelDisclosure, EndpointOptions, EntityDetails, TravelCreateModalDetails, TravelDisclosure, TravelDisclosureTraveller } from '../travel-disclosure-interface';
import { CommonService } from '../../common/services/common.service';
import { convertToValidAmount, deepCloneObject, openModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { TravelDataStoreService } from '../services/travel-data-store.service';
import { fadeInOutHeight } from '../../common/utilities/animations';
import { ElasticConfigService } from '../../common/services/elastic-config.service';
import { setEntityObjectFromElasticResult } from '../../common/utilities/elastic-utilities';
import { InformationAndHelpTextService } from '../../common/services/informationAndHelpText.service';

@Component({
    selector: 'app-travel-disclosure-form',
    templateUrl: './travel-disclosure-form.component.html',
    styleUrls: ['./travel-disclosure-form.component.scss'],
    animations: [fadeInOutHeight]
})
export class TravelDisclosureFormComponent implements OnInit, OnDestroy {

    deployMap: string = environment.deployUrl;
    datePlaceHolder: string = DATE_PLACEHOLDER;
    entitySearchOptions: any = {};
    countrySearchOptions: EndpointOptions;
    $subscriptions: Subscription[] = [];
    clearField = new String('true');
    countryClearField = new String('true');
    entityName = '';
    isInfoExpanded = true;
    mandatoryList = new Map();
    dateValidationList = new Map();
    travelDisclosureRO = new CoiTravelDisclosure();
    travelResObject = new TravelDisclosure();
    travellerTypeLookup: Array<TravelDisclosureTraveller>;
    travelStatusTypeLookup: Array<TravelDisclosureTraveller>;
    destination = null;
    travelSectionconfig: any = {};

    helpText = [
        'All the fields of travel disclosure form are mandatory.',
        'Fill in all the fields of the disclosure form and to save your progress, click on the ‘Save’ button.',
        'Navigate to the Certification section to certify and submit the disclosure.'
    ];
    isResultFromSearch = false;
    entityDetails: EntityDetails = new EntityDetails();
    addEntityConfirmation: any = null;
    isAddressReadMore: false;

    constructor(public commonService: CommonService,
        private _router: Router,
        private _service: TravelDisclosureService,
        private _dataStore: TravelDataStoreService,private _activatedRoute:ActivatedRoute,
        private _elasticConfig: ElasticConfigService,
        private _informationAndHelpTextService: InformationAndHelpTextService
    ) {
        window.scrollTo(0, 0);
    }

    ngOnInit(): void {
        this.getTravelSectionConfig();
        this.entitySearchOptions = this._elasticConfig.getElasticForActiveEntity();
        this.countrySearchOptions = getEndPointOptionsForCountry(this.commonService.fibiUrl);
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.loadTravellerTypesLookup();
        this.loadTravelStatusTypesLookup();
        this.handleTravelDisclosureSave();
        this.listenQueryParamsChanges();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private getDataFromStore(): void {
        if (this._dataStore.getData().travelDisclosureId) {
            this.setDisclosureDetails(this._dataStore.getData());
            this.entityDetails = this._dataStore.getEntityDetails();
        } else {
            this.getTravelCreateModalDetails();
        }
    }

    private setDisclosureDetails(responseObject: TravelDisclosure): void {
        this.clearField = new String('false');
        this.entitySearchOptions.defaultValue = responseObject.travelEntityName;
        this.entityName = responseObject.travelEntityName;
        this.countryClearField = new String('false');
        this.countrySearchOptions.defaultValue = responseObject.destinationCountry;
        this.destination = responseObject.destinationCountry ? 'International' : 'Domestic';
        this.travelDisclosureRO = this._dataStore.getTravelDisclosureRO();
        if (responseObject.travelEntityName) {
            this.isResultFromSearch = true;
        }
    }

    private handleTravelDisclosureSave(): void {
        this.$subscriptions.push(this._service.saveSubject.subscribe((event: string) => {
            if (event) {
                this.saveTravelDisclosure();
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
                this.getDataFromStore();
            })
        );
    }

    private getTravelCreateModalDetails(): void {
        const travelCreateModalDetails: TravelCreateModalDetails = this._dataStore.getCreateModalDetails();
        this.travelDisclosureRO.homeUnit = travelCreateModalDetails.homeUnit;
        this.travelDisclosureRO.description = travelCreateModalDetails.description;
        this.travelDisclosureRO.purposeOfTheTrip = travelCreateModalDetails.description;
        this.travelDisclosureRO.personId = travelCreateModalDetails.personId;
    }

    private getAllTravelDisclosureValues(requestObject: CoiTravelDisclosure): CoiTravelDisclosure {
        this.getTravellerTypeCode();
        this.setValuesForDestinationType();
        requestObject.isSponsoredTravel = true;
        requestObject.travelAmount = !requestObject.travelAmount ? null : convertToValidAmount(requestObject.travelAmount);
        requestObject.travelStartDate = parseDateWithoutTimestamp(requestObject.travelStartDate);
        requestObject.travelEndDate = parseDateWithoutTimestamp(requestObject.travelEndDate);
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
            this.clearField = new String('false');
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
        if (!this.destination) {
            this.mandatoryList.set('destination', 'Please choose a destination.');
        }
        if (this.destination === 'Domestic') {
            if (!this.travelDisclosureRO.travelState) {
                this.mandatoryList.set('state', 'Please enter the state.');
            }
        } else if (!this.travelDisclosureRO.destinationCountry) {
            this.mandatoryList.set('country', 'Please enter the country.');
            this.countryClearField = new String('false');
        }
        this.validateDates();
        return this.mandatoryList.size !== 0 || !this.validateDates() ? false : true;
}

    selectedEntityEvent(event: any): void {
        if (event) {
            event = setEntityObjectFromElasticResult(event);
            openModal('travel-entity-details');
        } else {
            this.clearEntity();
        }
        this.addEntityConfirmation = event ? event : null;
        this.setUnSavedChangesTrue();
    }

    clearEntity() {
        this.entityDetails = null;
        this.isResultFromSearch = false;
        this.entityName = null;
        this.travelDisclosureRO.entityId = null;
        this.travelDisclosureRO.entityNumber = null;
    }

    selectTravelCountry(event: any): void {
        this.travelDisclosureRO.destinationCountry = event ? event.countryName : null;
        this.setUnSavedChangesTrue();
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
        if (this.destination === 'Domestic') {
            this.travelDisclosureRO.destinationCountry = '';
            this.travelDisclosureRO.isInternationalTravel = false;
            this.countrySearchOptions.defaultValue = '';
        } else {
            this.travelDisclosureRO.travelState = '';
            this.travelDisclosureRO.isInternationalTravel = true;
        }
    }

    setUnSavedChangesTrue(): void {
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
        if (this.validateForm() && this._service.travelDataChanged) {
            this.$subscriptions.push(this._service.createCoiTravelDisclosure(this.travelDisclosureRO)
                .subscribe((res: any) => {
                    if (res) {
                        this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Travel Disclosure Saved Successfully');
                        this._service.setUnSavedChanges(false, '');
                        this._dataStore.removeCreateModalDetails();
                        this.updateTravelDataStore(res);
                    }
                }, (err) => {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in Saving Travel Disclosure');
                })
            );
        }
    }

    private updateTravelDataStore(response: any) {
        this.travelResObject = response;
        this.travelResObject.homeUnitName = response.travellerUnitDetails.unitName;
        this.travelResObject.homeUnitNumber = response.travellerUnitDetails.unitNumber;
        this.travelResObject.travelEntityName = response.entityDetails.entityName;
        this.travelResObject.entityType = response.entityDetails.entityType.description;
        this.travelResObject.entityTypeCode = response.entityDetails.entityType.entityTypeCode;
        this.travelResObject.countryCode = response.entityDetails.country.countryCode;
        this.travelResObject.country = response.entityDetails.country.countryName;
        this.travelResObject.reviewStatus = response.coiTravelReviewStatusTypeDetails.description;
        this.travelResObject.reviewStatusCode = response.coiTravelReviewStatusTypeDetails.reviewStatusCode;
        this.travelResObject.documentStatus = response.coiDocumentStatusTypeDetalis.description;
        this.travelResObject.documentStatusCode = response.coiDocumentStatusTypeDetalis.documentStatusCode;
        this.travelResObject.adminGroupId = response.adminGroupId;
        this.travelResObject.adminGroupName = response.adminGroupName;
        this.travelResObject.adminPersonId = response.adminPersonId;
        this.travelResObject.adminPersonName = response.adminPersonName;
        this.travelResObject.travelState = response.travelstate;
        this.travelResObject.disclosureStatus = response.coiTravelDisclosureStatusTypeDetalis.description;
        this.travelResObject.disclosureStatusCode = response.coiTravelDisclosureStatusTypeDetalis.disclosureStatusCode;
        this._dataStore.manualDataUpdate(this.travelResObject);
        this._router.navigate([], {
            queryParams: {
                disclosureId: this.travelResObject.travelDisclosureId
            }
        });
    }

    viewEntity(entityId: string): void {
        this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: entityId } });
    }

    confirmEntityDetails() {
        this.entityDetails = deepCloneObject(this.addEntityConfirmation);
        this.isResultFromSearch = true;
        this.entityName = this.entityDetails.entityName;
        this.travelDisclosureRO.entityId = this.entityDetails.entityId;
        this.travelDisclosureRO.entityNumber = this.entityDetails.entityNumber;
        this.addEntityConfirmation = null;
    }

    clearEntityDetails() {
        this.clearField = new String('true');
        this.countryClearField = new String('true');
        this.addEntityConfirmation = null;
        this.entityDetails = null;
        this.isResultFromSearch = false;
        this.entityName = null;
        this.travelDisclosureRO.entityId = null;
        this.travelDisclosureRO.entityNumber = null;
        this.entitySearchOptions.defaultValue = '';
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

    private listenQueryParamsChanges() {
        this.$subscriptions.push(this._activatedRoute.queryParams.subscribe(params => {
            const MODULE_ID = params['disclosureId'];
            if (!MODULE_ID) {
                this.travelDisclosureRO = new CoiTravelDisclosure();
                this.entitySearchOptions = this._elasticConfig.getElasticForActiveEntity();
                this.countrySearchOptions = getEndPointOptionsForCountry(this.commonService.fibiUrl);
                this.loadTravellerTypesLookup();
                this.loadTravelStatusTypesLookup();
                this.destination = null;
                this.getTravelCreateModalDetails();
                this.clearEntity();
            }
        }));
    }

    getTravelSectionConfig(){
        this.travelSectionconfig = this._activatedRoute.snapshot.data.moduleConfig;
        this._informationAndHelpTextService.moduleConfiguration = this.commonService.getSectionCodeAsKeys(this.travelSectionconfig);   
    }

}


