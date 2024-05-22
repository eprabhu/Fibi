import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { DATE_PLACEHOLDER } from '../../../src/app/app-constants';
import { getEndPointOptionsForEntity, getEndPointOptionsForCountry } from '../../../../fibi/src/app/common/services/end-point.config';
import { deepCloneObject, isEmptyObject, openModal } from '../../../../fibi/src/app/common/utilities/custom-utilities';
import { compareDates, parseDateWithoutTimestamp } from '../../../../fibi/src/app/common/utilities/date-utilities';
import { environment } from '../../environments/environment';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../app-constants';
import { CommonService } from '../common/services/common.service';
import { NavigationService } from '../common/services/navigation.service';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { CoiEntity, EntityDetails } from '../entity-management/entity-details-interface';


declare const $: any;
export interface EndpointOptions {
    contextField: string;
    formatString: string;
    path: string;
    defaultValue: string;
    params: string;
}
@Component({
    selector: 'app-add-sfi',
    templateUrl: './add-sfi.component.html',
    styleUrls: ['./add-sfi.component.scss']
})
export class AddSfiComponent implements OnInit {
    isSaving = false;
    entityDetails: EntityDetails = new EntityDetails();
    additionalDetails: any = {
        sponsorsResearch: false
    };
    deployMap = environment.deployUrl;
    isAddAttachment = false;
    isAddAssignee = false;
    dateTime: string;
    datePlaceHolder = DATE_PLACEHOLDER;
    isReadMore: false;
    clearField: any = false;
    EntitySearchOptions: any = {};
    countrySearchOptions: EndpointOptions;
    clearCountryField: any;
    $subscriptions: Subscription[] = [];
    mandatoryList = new Map();
    emailWarningMsg: any;
    sfiLookUpList: any = {};
    isExpandedAdditionalDetails = true;
    isResultFromSearch = false;
    riskLevelLookup = [];
    isEntityManagement = false;
    heading: string;
    buttonName: string;
    btnTitle = '';
    isViewMode: any;
    sfiType: string;
    existingEntityDetails: any = {};
    canShowEntityFields = false;
    addEntityConfirmation: any = null;
    isAddressReadMore: false;
    isChecked = {};
    relationLookup: any = [];
    concurrencyPersonEntityId = null;
    involvementDate =  {
        involvementStartDate: null,
        involvementEndDate: null
    }

    @Output() emitUpdateEvent = new EventEmitter<number>();
    @Input() modifyType = '';
    @Input() disclosureDetails: { disclosureId: any, disclosureNumber: any } = { disclosureId: null, disclosureNumber: null };
    @Input() coiEntityManageId: any = null;
    @Input() isEditEntity = false;
    @Input() isSlider = false;
    @Input() revisionReason = '';

    constructor(public sfiService: SfiService, private _activatedRoute: ActivatedRoute,
        public _commonService: CommonService, private _router: Router, public _navigationService: NavigationService) { }

    ngOnInit(): void {
        this.isEntityManagement = this._router.url.includes('entity-management') || this.checkIsEntityTypeInURL();
        this.setHeader();
        this.getSFILookup();
        this.setDefaultRiskLevel();
        this.getRelationshipLookUp();
        if (this.coiEntityManageId) {
            this.getEntityDetails();
        }
        if (this.isEntityManagement) {
            this.canShowEntityFields = true;
        }
        this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonService.baseUrl, 'ONLY_ACTIVE');
        this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
        window.scrollTo(0,0);
    }

    private checkIsEntityTypeInURL(): boolean {
        let isEntityTypeInURL = false;
        this.$subscriptions.push(this._activatedRoute.queryParams.subscribe(params => {
            isEntityTypeInURL = params['type'] ? params['type'] === 'SFI' ? false : true : false;
            this.sfiType = params['type'];
        }));
        return isEntityTypeInURL;
    }

    private getSFILookup(): void {
        this.$subscriptions.push(this.sfiService.addSFILookUp().subscribe((res: any) => {
            this.sfiLookUpList = res;
            this.riskLevelLookup = res.entityRiskCategories;
        }));
    }

    setEntityTypeObj(): void {
        this.entityDetails.coiEntity.entityType = this.sfiLookUpList.entityType.find(ele =>
            this.entityDetails.coiEntity.entityTypeCode === ele.entityTypeCode);
    }

    hideRelationshipModal(event): void {
        this.clearSFIFields();
        this.clearField = new String('true');
        this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonService.baseUrl, 'ONLY_ACTIVE');
    }

    private createOrUpdateEntitySFI(): void {
        this.entityDetails.coiEntity.entityId && !this.isEntityManagement ?
            this.saveAdditionalDetails() : this.saveEntityDetails();
    }

    saveEntityDetails(): void {
        this.entityDetails.coiEntity.entityStatusCode = this.getEntityStatusCode();
        this.$subscriptions.push(this.sfiService.saveOrUpdateCoiEntity(this.entityDetails).subscribe((data: CoiEntity) => {
            this.entityDetails.coiEntity = data;
            this.entityDetails.coiEntity.entityId && !this.isEntityManagement ?
                this.saveAdditionalDetails() : this.updateDetails();
        }, _err => {
            this.isSaving = false;
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }

    private getEntityStatusCode() {
        return (this._router.url.includes('entity-management') || this.sfiType === 'ENTITY') ? '1' : '2';
    }

    private checkIfSFIAlreadyAdded(entityId, event): void {
        this.mandatoryList.delete('entityAlreadyAdded');
        this.$subscriptions.push(this.sfiService.isEntityAdded(entityId).subscribe((res: any) => {
            if (res) {
                this.existingEntityDetails = res;
                if (this.existingEntityDetails.personEntityRelationships.length) {
                    this.existingEntityDetails.personEntityRelationships = this.groupByDisclosureType(deepCloneObject( this.existingEntityDetails.personEntityRelationships), "coiDisclosureType", "description", "validPersonEntityRelType");
                } else {
                    this.existingEntityDetails.personEntityRelationships = {};
                }
                this.mandatoryList.set('entityAlreadyAdded', 'An SFI has already been created against the entity you are trying to add. To view the SFI, please click on the View button on the SFI card.');
            } else {
                openModal('entity-details');
                this.addEntityConfirmation = event;
                if (event.country) {
                    this.countrySearchOptions.defaultValue = event.country.countryName;
                    this.selectedCountryEvent(event.country);
                }
                this.clearCountryField = new String('false');
            }
        }, err => {
            this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonService.baseUrl, 'ONLY_ACTIVE');
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Entity selection failed. Please try again');
        }));
    }

    updateDetails(): void {
        this.emitUpdateEvent.emit(this.entityDetails.coiEntity.entityId);
        this.sfiService.isShowSfiNavBar = false;
        this.isSaving = false;
        if (this.modifyType) {
            $('#actionConfirmationModal').modal('hide');
        }
        if (!this.isSlider) {
            this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: this.entityDetails.coiEntity.entityId } });
        }
        this._commonService.showToast(HTTP_SUCCESS_STATUS, `Entity ${this.isEditEntity ? 'updated ' : 'created '} successfully.`);
    }

    setDateValues() {
        this.additionalDetails.involvementStartDate = parseDateWithoutTimestamp(this.involvementDate.involvementStartDate);
        this.additionalDetails.involvementEndDate = parseDateWithoutTimestamp(this.involvementDate.involvementEndDate);
    }

    private saveAdditionalDetails(): void {
        this.setDateValues();
        this.$subscriptions.push(this.sfiService.createSFI(
            {
                entityId: this.entityDetails.coiEntity.entityId,
                entityNumber: this.entityDetails.coiEntity.entityNumber,
                ...this.additionalDetails,
                "validPersonEntityRelTypeCodes": this.getSelectedRelationTypeCodes().map(typeCode => Number(typeCode))
            }).subscribe((data: any) => {
                if (data) {
                    this.additionalDetails = data.personEntity;
                    this.isSaving = false;
                    this.navigateToSFI(data.personEntityId);
                }
                this._commonService.showToast(HTTP_SUCCESS_STATUS, 'SFI saved successfully.');
            }, _err => {
                this.isSaving = false;
                    if (_err.status === 405) {
                        this.concurrencyPersonEntityId = _err.error.personEntityId;
                        openModal('coi-add-sfi-concurrency-modal');
                    } else {
                        this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving SFI. Please try again.');
                    }
            }));
    }

    validateRelationship(elementIdList) {
        if (!this.getSelectedRelationTypeCodes().length) {
            this.mandatoryList.set('relationRadio', 'Please select atleast one relationship.');
            elementIdList.push('relation-radio-btn');
        }
    }

    checkForNotEmpty(val) {
       return !isEmptyObject(val);
    }

    selectNewEntity(event): void {
        this.clearSFIFields();
        this.EntitySearchOptions.defaultValue = event.searchString;
        this.clearField = new String('false');
        this.entityDetails.coiEntity.entityName = event.searchString;
        this.canShowEntityFields = true;
    }

    selectedEvent(event): void {
        this.canShowEntityFields = false;
        this.clearSFIFields();
        if (event) {
            this.clearField = new String('false');
            this.checkIfSFIAlreadyAdded(event.entityId, event);
        } else {
            this.sfiService.$addRelationService.next(null);
        }
    }

    selectedCountryEvent(event): void {
        if (event) {
            this.entityDetails.coiEntity.countryCode = event.countryCode;
            this.countrySearchOptions.defaultValue = event.countryName;
        } else {
            this.entityDetails.coiEntity.countryCode = null;
        }
    }

    private clearSFIFields(): void {
        this.entityDetails = new EntityDetails();
        this.additionalDetails = {
            sponsorsResearch: false
        };
        this.clearCountryField = new String('true');
        this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
        this.isResultFromSearch = false;
        this.mandatoryList.clear();
    }

    private checkMandatoryFilled(): boolean {
        this.mandatoryList.clear();
        const ELEMENT_ID_LIST = [];
        if (!this.entityDetails.coiEntity.entityName) {
            this.mandatoryList.set('entityName', 'Please enter Entity Name.');
            ELEMENT_ID_LIST.push('entity-name');
        }
        if (!this.isResultFromSearch) {
            this.entityDetailsValidation(ELEMENT_ID_LIST);
        }
        if (!this.isEntityManagement) {
            if (!this.involvementDate.involvementStartDate) {
                this.mandatoryList.set('date', 'Please enter Start Date.');
                ELEMENT_ID_LIST.push('coi-add-sfi-start-date-involvement')
            }
            this.endDateValidation(ELEMENT_ID_LIST);
            this.validateRelationship(ELEMENT_ID_LIST);
            if (!this.additionalDetails.staffInvolvement) {
                this.mandatoryList.set('staff', 'Please enter Relationship with Entity.');
                ELEMENT_ID_LIST.push('coi-add-sfi-releationship-entity')
            }
            if (!this.additionalDetails.studentInvolvement) {
                this.mandatoryList.set('student', 'Please enter Principle Business Area of Entity.');
                ELEMENT_ID_LIST.push('coi-add-sfi-student-entity')
            }
            if (!this.additionalDetails.instituteResourceInvolvement) {
                this.mandatoryList.set('resource', 'Please enter Relationship of Entity to your University responsibilities.');
                ELEMENT_ID_LIST.push('coi-add-sfi-resource-sfi');
            }
        }
        this.focusValidationField(ELEMENT_ID_LIST)
        return this.mandatoryList.size !== 0 ? false : true;
    }

    private entityDetailsValidation(elementIdList): void {
        if (!this.entityDetails.coiEntity.countryCode) {
            this.mandatoryList.set('country', 'Please enter Country.');
            elementIdList.push('coi-add-sfi-country-search');
        }
        if (!this.entityDetails.coiEntity.address) {
            this.mandatoryList.set('address', 'Please enter Address.');
            elementIdList.push('coi-add-sfi-address-textarea');
        }
        this.emailValidation(elementIdList);
        if (!this.entityDetails.coiEntity.entityTypeCode || this.entityDetails.coiEntity.entityTypeCode === 'null') {
            this.mandatoryList.set('entityType', 'Please enter Entity Type.');
            elementIdList.push('coi-add-sfi-entity-type');
        }  
    }

    private emailValidation(elementIdList): void {
        this.emailWarningMsg = null;
        if (this.entityDetails.coiEntity.emailAddress) {
            this.entityDetails.coiEntity.emailAddress = this.entityDetails.coiEntity.emailAddress.trim();
            if (this.entityDetails.coiEntity.emailAddress !== undefined && this.entityDetails.coiEntity.emailAddress !== '') {
                const email = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)| (".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
                if (!(email.test(String(this.entityDetails.coiEntity.emailAddress).toLowerCase()))) {
                    this.emailWarningMsg = 'Please select a valid email address.';
                    elementIdList.push('email-input-box')
                } else {
                    this.emailWarningMsg = null;
                }
            }
        }
    }

    inputRestriction(event: any) {
        const pattern = /[0-9\+\-\/\ ]/;
        if (!pattern.test(String.fromCharCode(event.charCode))) {
            event.preventDefault();
        }
    }

    phoneNumberValidation(input) {
        this.mandatoryList.delete('phoneNumber');
        // tslint:disable-next-line:max-line-length
        const pattern = (/^(?:(?:\(?(?:00|\+)([1-4]\d\d|[1-9]\d?)\)?)?[0-9]?)?((?:\(?\d{1,}\)?[\-\.\ \\\/]?){0,})(?:[\-\.\ \\\/]?(?:#|ext\.?|extension|x)[\-\.\ \\\/]?(\d+))?$/);
        if (!pattern.test(input)) {
            this.checkForInvalidPhoneNumber(input);
        }
    }

    private checkForInvalidPhoneNumber(input): void {
        if (/^([a-zA-Z]|[0-9a-zA-Z])+$/.test(input)) {
            this.mandatoryList.set('phoneNumber', 'Alphabets cannot be added in Phone number field.');
        } else {
            this.mandatoryList.set('phoneNumber', 'Please add a valid number');
        }
    }

    endDateValidation(elementIdList): void {
        this.mandatoryList.delete('endDate');
        if (this.involvementDate.involvementStartDate && this.involvementDate.involvementEndDate &&
            (compareDates(this.involvementDate.involvementStartDate, this.involvementDate.involvementEndDate) === 1)) {
            this.mandatoryList.set('endDate', 'Please provide a valid end date.');
            elementIdList.push('end-date-involvement')
        }
    }

    setHeader(): void {
        if (this.isEntityManagement) {
            if (this.isEditEntity) {
                this.buttonName = 'Update Entity';
                this.btnTitle = 'Click here to update entity';
                this.heading = `Entity ${this.entityDetails.coiEntity.entityName}`;
            } else {
                this.heading = 'Add New Entity';
                this.buttonName = 'Create Entity';
                this.btnTitle = 'Click here to create entity';
            }
        } else {
            this.heading = 'Significant Financial Interest';
            this.buttonName = 'Save';
            this.btnTitle = 'Click here to save SFI';
        }
    }

    private getEntityDetails(): void {
        this.$subscriptions.push(this.sfiService.getEntityDetails(this.coiEntityManageId).subscribe((res: EntityDetails) => {
            this.entityDetails = res;
            this.heading = `Entity ${this.entityDetails.coiEntity.entityName}`;
            this.clearCountryField = new String('false');
            this.countrySearchOptions.defaultValue = this.entityDetails.coiEntity.country.countryName;
            this.selectedCountryEvent(res.coiEntity.country);
        }));
    }

    setEntityRiskCategoryObj(): void {
        this.entityDetails.coiEntity.entityRiskCategory = this.riskLevelLookup.find(ele =>
            this.entityDetails.coiEntity.riskCategoryCode === ele.riskCategoryCode);
    }

    submitEntity(): void {
        if (this.mandatoryList.has('entityAlreadyAdded') || ((!this.checkMandatoryFilled() || this.emailWarningMsg) && !this.isSaving)) {
            return;
        }
        this.modifyType == '1' ? $('#actionConfirmationModal').modal('show') : this.updateEntityDetails();
    }

    backToPreviousPage(): void {
        if (this._navigationService.previousURL) {
            this._router.navigateByUrl(this._navigationService.previousURL);
        } else {
            this._router.navigate(['/coi/user-dashboard']);
        }
    }

    private setDefaultRiskLevel(): void {
        this.entityDetails.coiEntity.riskCategoryCode = '3';
    }

    updateEntityDetails() {
        this.entityDetails.coiEntity.revisionReason = this.revisionReason;
        this.entityDetails.coiEntity.majorVersion = this.modifyType === '2' ? true : false;
        this.createOrUpdateEntitySFI();
    }

    viewSfiDetails() {
        this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: this.existingEntityDetails.personEntityId,personEntityNumber: this.existingEntityDetails.entityNumber } });
    }

    editSfiDetails(personEntityId) {
        this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: personEntityId, mode: 'E', personEntityNumber: this.existingEntityDetails.entityNumber } });
      }

    viewEntityDetails(event) {
        this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: event } });
    }

    confirmEntityDetails() {
        this.isResultFromSearch = true;
        this.entityDetails.coiEntity = deepCloneObject(this.addEntityConfirmation);
        this.addEntityConfirmation = null;
    }

    clearEntityDetails() {
        this.clearField = new String('true');
        this.addEntityConfirmation = null;
        this.canShowEntityFields = false;
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


    getSelectedRelationTypeCodes() {
        return Object.keys(this.isChecked).filter(key => this.isChecked[key]);
    }

    getRelationshipLookUp() {
        this.$subscriptions.push(this.sfiService.addSFILookUp().subscribe((res: any) => {
            if(res) {
                this.relationLookup = this.groupBy(res.validPersonEntityRelTypes, "coiDisclosureType", "description");
            }
        }));
    }

    groupBy(jsonData, key, innerKey) {
        return jsonData.reduce((relationsTypeGroup, item) => {
            (relationsTypeGroup[item[key][innerKey]] = relationsTypeGroup[item[key][innerKey]] || []).push(item);
            return relationsTypeGroup;
        }, {});
    }

    groupByDisclosureType(jsonData: any, coiDisclosureType: any, description: any, validPersonEntityRelType: any): {} {
        return jsonData.reduce((relationsTypeGroup, item) => {
            (relationsTypeGroup[item[validPersonEntityRelType][coiDisclosureType][description]] = relationsTypeGroup[item[validPersonEntityRelType][coiDisclosureType][description]] || []).push(item);
            return relationsTypeGroup;
        }, {});
    }

    navigateToSFI(personEntityId) {
        this.sfiService.isShowSfiNavBar = false;
        document.body.removeAttribute("style");
        this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: personEntityId, personEntityNumber: this.existingEntityDetails.entityNumber } });
    }

    goToHome() {
        this._router.navigate(['/coi/user-dashboard']);
    }

    navigateBack() {
        this._router.navigateByUrl(this._navigationService.previousURL);
    }

    focusValidationField(elementList) {
        if(elementList.length) {
            const ELEMENT: HTMLElement = document.getElementById(elementList[0]);
            const OFFSET_HEADER = document.getElementById('create-sfi-header')?.clientHeight;
            const SECTION_HEIGHT = ELEMENT.offsetTop - OFFSET_HEADER;
            if (document.activeElement.id != elementList[0]) {
                this.isSlider ? document.getElementById('add-sfi').scrollTo({ behavior: 'smooth', top: SECTION_HEIGHT }) :window.scrollTo({ behavior: 'smooth', top: SECTION_HEIGHT });
            }
            ELEMENT.focus();
        }
    }

}
