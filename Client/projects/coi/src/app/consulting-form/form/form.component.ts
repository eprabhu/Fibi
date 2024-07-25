import { Component } from '@angular/core';
import { ElasticConfigService } from '../../common/services/elastic-config.service';
import { EndpointOptions } from '../../add-sfi/add-sfi.component';
import { getEndPointOptionsForCountry } from '../../configuration/form-builder-create/shared/form-builder-view/search-configurations';
import { CommonService } from '../../common/services/common.service';
import { EntityDetails, EntitySaveRO, ExistingEntityDetails } from './form-interface';
import { setEntityObjectFromElasticResult } from '../../common/utilities/elastic-utilities';
import { Subscription } from 'rxjs';
import { FormService } from './form-service.service';
import { deepCloneObject, isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { Router } from '@angular/router';
import { ConsultingService } from '../services/consulting-service.service';
import { FBConfiguration } from '../../configuration/form-builder-create/shared/form-builder-view/form-builder-interface';
import { DataStoreService } from '../services/data-store.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { HttpResponse } from '@angular/common/http';
import { parseDateWithoutTimestamp } from '../../common/utilities/date-utilities';

@Component({
    selector: 'app-form',
    templateUrl: './form.component.html',
    styleUrls: ['./form.component.scss']
})
export class FormComponent {

    entitySearchOptions: any = {};
    clearField: any = false;
    mandatoryList = new Map();
    isNewEntityFromSearch = false;
    canShowEntityFields = false;
    clearCountryField: any = false;
    countrySearchOptions: EndpointOptions;
    entityDetails: EntityDetails = new EntityDetails();
    sfiLookUpList: any = {};
    $subscriptions: Subscription[] = [];
    exisitngSFIForEntity: ExistingEntityDetails = new ExistingEntityDetails();
    addEntityConfirmation: any = null;
    isResultFromSearch = false;
    consultingForm: any = {};
    fbConfiguration = new FBConfiguration();
    isFormEditMode = this.dataStore.isFormEditable();
    emailWarningMsg = '';
    entityDetailsAlreadySave: any;
    showTextAreaLimiter: boolean;

    constructor(private _elasticConfig: ElasticConfigService, public commonService: CommonService, private _formService: FormService,
        private _router: Router, public consultingService: ConsultingService, public dataStore: DataStoreService
    ) { }

    ngOnInit() {
        this.entitySearchOptions = this._elasticConfig.getElasticForActiveEntity();
        this.countrySearchOptions = getEndPointOptionsForCountry(this.commonService.fibiUrl);
        this.getSFILookup();
        this.saveSubscribe();
        this.getDataFromStore();
        this.listenToDataChange();
        window.scrollTo(0, 0);
    }

    selectedEvent(event: any): void {
        this.canShowEntityFields = false;
        this.consultingService.isDataChangeAvailableInEntity = true;
        if (event) {
            this.clearField = new String('false');
            event = setEntityObjectFromElasticResult(event);
            this.checkIfSFIAlreadyAdded(event.entityNumber, event);
            this.mandatoryList.clear();
        } else {
            this.isResultFromSearch = false;
            this.entityDetails = new EntityDetails();
            this.checkForSubmitDisable();
        }
    }

    addNewEntity(event: string): void {
        this.clearSFIFields();
        this.isNewEntityFromSearch = true;
        this.entityDetails.coiEntity.entityName = event;
        this.entitySearchOptions.defaultValue = event;
        this.consultingService.isDataChangeAvailableInEntity = true;
        this.canShowEntityFields = true;
        this.showTextAreaLimiter = true;
        this.consultingService.canDisableSubmit = true;
    }

    selectedCountryEvent(event: any): void {
        if (event) {
            this.entityDetails.coiEntity.countryCode = event.countryCode;
            this.countrySearchOptions.defaultValue = event.countryName;
        } else {
            this.entityDetails.coiEntity.countryCode = null;
        }
    }

    private getSFILookup(): void {
        this.$subscriptions.push(this._formService.addSFILookUp().subscribe((res: any) => {
            this.sfiLookUpList = res;
        }));
    }

    setEntityTypeObj(): void {
        this.entityDetails.coiEntity.entityType = this.sfiLookUpList.entityType.find(ele =>
            this.entityDetails.coiEntity.entityTypeCode === ele.entityTypeCode);
    }

    private checkIfSFIAlreadyAdded(entityNumber: number, event: any): void {
        this.exisitngSFIForEntity.isEntityAvailable = false;
        this.$subscriptions.push(this._formService.isEntityAdded(entityNumber).subscribe((res: HttpResponse<any>) => {
            if (res.status == 200) {
                this.setExistingEntityDetails(res.body);
            }
            openModal('entity-details');
            this.addEntityConfirmation = event;
            if (event.country) {
                this.countrySearchOptions.defaultValue = event.country.countryName;
                this.selectedCountryEvent(event.country);
            }
            this.clearCountryField = new String('false');
        }, err => {
            this.entitySearchOptions = this._elasticConfig.getElasticForActiveEntity();
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Entity selection failed. Please try again');
        }));
    }

    private setExistingEntityDetails(response: any): void {
            this.exisitngSFIForEntity.personEntityId = response.personEntityId;
            this.exisitngSFIForEntity.entityNumber = response.entityNumber;
            this.exisitngSFIForEntity.personEntityRelationships = response.personEntityRelationships;
            this.exisitngSFIForEntity.isEntityAvailable = true;
    }

    confirmEntityDetails(): void {
        this.isResultFromSearch = true;
        this.entityDetails.coiEntity = deepCloneObject(this.addEntityConfirmation);
        this.addEntityConfirmation = null;
        this.checkForSubmitDisable();
        this.setHeaderEntityName(this.entityDetails.coiEntity);
    }

    clearEntityDetails(): void{
        this.clearField = new String('true');
        this.addEntityConfirmation = null;
        this.canShowEntityFields = false;
        this.resetEntity();
        this.checkForSubmitDisable();
        this.setHeaderEntityName(this.entityDetails.coiEntity);
    }

    resetEntity() {
        if (this.entityDetailsAlreadySave && !isEmptyObject(this.entityDetailsAlreadySave)) {
            this.entityDetails = deepCloneObject(this.entityDetailsAlreadySave);
            this.isResultFromSearch = true;
            this.resetEntityDefaultValue(this.entityDetailsAlreadySave.coiEntity.entityName);
        }
    }

    resetEntityDefaultValue(entityName) {
        this.clearField = new String('false');
        this.entitySearchOptions.defaultValue = entityName;
    }

    private clearSFIFields(): void {
        this.entityDetails = new EntityDetails();
        this.clearCountryField = new String('true');
        this.countrySearchOptions = getEndPointOptionsForCountry(this.commonService.fibiUrl);
        this.isResultFromSearch = false;
        this.mandatoryList.clear();
        this.isNewEntityFromSearch = false;
        this.exisitngSFIForEntity = new ExistingEntityDetails();
        this.entitySearchOptions = this._elasticConfig.getElasticForActiveEntity();
    }

    viewEntity(entityId: string): void {
        this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: entityId } });
    }

    /**four scenarios for form entity save
     * 1. If new entity, save entity details, create sFI with entity, then consulting form entity will be saved.
     * 2. If entity is already available, but no SFI is available, then SFI will created and then consulting form entity will be saved.
     * 3. If SFI is already avalilable for Entity, and no Consulting relation, first relation is added then consulting form entity will be saved.
     * 4. If SFI is already avalilable for Entity, with Consulting relation, just consulting form entity will be saved.
     **/
    private saveSubscribe(): void {
        this.$subscriptions.push(this.consultingService.globalSave$.subscribe((data: any) => {
            this.mandatoryList.clear();
            if (!this.entityDetails.coiEntity.entityName) {
                this.mandatoryList.set('entityName', 'Please add entity name');
            }
            if(!this.mandatoryList.size) {
                if (this.consultingService.isDataChangeAvailableInEntity) {
                    if (this.canShowEntityFields) {
                        this.addNewEntityInitally();
                    } else {
                        this.saveFormForExistingSFI();
                    }
                }
            }
        }));
    }

    private saveFormForExistingSFI(): void {
        this.mandatoryList.clear();
        if (!this.mandatoryList.size) {
            if (this.exisitngSFIForEntity.isEntityAvailable) {
                const CHECK_FOR_CONSULTING = !!this.exisitngSFIForEntity.personEntityRelationships.find(ele => ele.validPersonEntityRelTypeCode == 7);
                if (!CHECK_FOR_CONSULTING) {
                    this.updateEntityWithConsultingRelationship(this.exisitngSFIForEntity.personEntityId);
                } else {
                    this.saveFormPersonEntity(this.exisitngSFIForEntity.personEntityId, this.exisitngSFIForEntity.entityNumber);
                }
            } else {
                this.createNewSFIWithEntity();
            }
        }
    }

    private updateEntityWithConsultingRelationship(personEntityId): void {
        this.$subscriptions.push(this._formService.saveOrUpdateCoiFinancialEntityDetails({
            'questionnaireAnsHeaderId': null,
            'personEntityId': personEntityId,
            'validPersonEntityRelTypeCodes': [7]
        }).subscribe((data: any) => {
            if (data) {
                this.saveFormPersonEntity(data.personEntityId, data.entityNumber);
            }
        }));
    }

    private entityDetailsValidation(): void {
        if (!this.entityDetails.coiEntity.countryCode) {
            this.mandatoryList.set('country', 'Please enter Country.');
        }
        if (!this.entityDetails.coiEntity.address) {
            this.mandatoryList.set('address', 'Please enter Address.');
        }
        this.emailValidation();
        if (!this.entityDetails.coiEntity.entityTypeCode || this.entityDetails.coiEntity.entityTypeCode === 'null') {
            this.mandatoryList.set('entityType', 'Please enter Entity Type.');
        }
    }

    private emailValidation(): void {
        this.emailWarningMsg = null;
        if (this.entityDetails.coiEntity.emailAddress) {
            this.entityDetails.coiEntity.emailAddress = this.entityDetails.coiEntity.emailAddress.trim();
            if (this.entityDetails.coiEntity.emailAddress !== undefined && this.entityDetails.coiEntity.emailAddress !== '') {
                const email = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)| (".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
                if (!(email.test(String(this.entityDetails.coiEntity.emailAddress).toLowerCase()))) {
                    this.emailWarningMsg = 'Please select a valid email address.';
                } else {
                    this.emailWarningMsg = null;
                }
            }
        }
    }

    private addNewEntityInitally(): void {
        this.mandatoryList.clear();
        this.entityDetailsValidation();
        if (!this.mandatoryList.size && !this.emailWarningMsg) {
            this.entityDetails.coiEntity.entityStatusCode = '2';
            this.$subscriptions.push(this._formService.saveOrUpdateCoiEntity(this.entityDetails).subscribe((data: any) => {
                if (data) {
                    this.entityDetails.coiEntity = data;
                    this.createNewSFIWithEntity();
                }
            }))
        }
    }

    private createNewSFIWithEntity(): void {
        this.$subscriptions.push(this._formService.savePersonEntity(this.saveRO(this.entityDetails.coiEntity)).subscribe((data: any) => {
            if (data) {
                this.saveFormPersonEntity(data.personEntityId, data.entityNumber);
                this.setExistingEntityDetails(data);
            }
        }))
    }

    private saveRO(entity): any {
        const entitySaveRO = new EntitySaveRO();
        entitySaveRO.entityId = entity.entityId;
        entitySaveRO.entityNumber = entity.entityNumber;
        entitySaveRO.involvementStartDate = parseDateWithoutTimestamp(new Date());
        entitySaveRO.validPersonEntityRelTypeCodes = [7];
        return entitySaveRO;
    }

    ngAfterViewInit(): void {
        if (this.consultingForm.consultingFormDisclosure) {
            this.updateFormConfiguration(this.consultingForm.consultingFormDisclosure.disclosureId.toString(),
                                        this.consultingForm.consultingFormDisclosure.personId,
                                        this.consultingForm.consultingFormDisclosure.consultingDisclFormBuilderDetails[0].formBuilderId);
        }
        this.updateFormEditMode();
    }

    private updateFormConfiguration(disclId, personId, formBuilderId): void {
        this.fbConfiguration.moduleItemCode = '27';
        this.fbConfiguration.moduleSubItemCode = '0';
        this.fbConfiguration.moduleItemKey = disclId;
        this.fbConfiguration.moduleSubItemKey = '0';
        this.fbConfiguration.documentOwnerPersonId = personId;
        this.fbConfiguration.formBuilderId = formBuilderId;
        this.consultingService.formBuilderEvents.next({ eventType: 'CONFIGURATION', data: this.fbConfiguration });
    }

    private listenToDataChange(): void {
        this.$subscriptions.push(this.dataStore.dataEvent.subscribe((res) => {
            this.getDataFromStore();
            this.updateFormEditMode();
        }));
    }

    private updateFormEditMode(): void{
        const latestIsFormEditMode = this.dataStore.isFormEditable();
        this.consultingService.formBuilderEvents.next({ eventType: 'IS_EDIT_MODE', data: latestIsFormEditMode });
        this.isFormEditMode = latestIsFormEditMode;
    }

    private getDataFromStore(): void {
        const DATA = this.dataStore.getData();
        if(this.consultingForm.consultingFormDisclosure && (DATA.consultingFormDisclosure.disclosureId != this.consultingForm.consultingFormDisclosure.disclosureId)) {
            this.updateFormConfiguration(DATA.consultingFormDisclosure.disclosureId.toString(),
                                         DATA.consultingFormDisclosure.personId,
                                         DATA.consultingFormDisclosure.consultingDisclFormBuilderDetails[0].formBuilderId);
        }
        this.consultingForm = DATA;
        this.setEntityDetails();
    }

    private setEntityDetails(): void {
        if (this.consultingForm.consultingFormDisclosure.personEntity) {
            this.entityDetails = deepCloneObject(this.consultingForm.consultingFormDisclosure.personEntity);
            this.entityDetailsAlreadySave = deepCloneObject(this.consultingForm.consultingFormDisclosure.personEntity);
            this.resetEntityDefaultValue(this.entityDetailsAlreadySave.coiEntity.entityName);
            this.isResultFromSearch = true;
            this.setHeaderEntityName(this.entityDetails.coiEntity);
        } else {
            this.entityDetails = new EntityDetails();
            this.entityDetailsAlreadySave = null;
            this.entitySearchOptions = this._elasticConfig.getElasticForActiveEntity();
            this.isResultFromSearch = false;
        }
        this.checkForSubmitDisable();
    }

    setHeaderEntityName(coiEntity: any) {
        this.consultingService.coiEntity = coiEntity;
    }

    // commentSliderEvent(event) {
    //     const COMMENT_META_DATA: any = {
    //         documentOwnerPersonId: this.consultingForm.consultingFormDisclosure..personId,
    //         componentTypeCode: event.componentTypeCode,
    //         formBuilderComponentId: event.formBuilderComponentId,
    //         formBuilderId : event.formBuilderId,
    //         formBuilderSectionId : event.formBuilderSectionId,
    //         headerName: event.headerName
    //     }
    //     this.commonService.$commentConfigurationDetails.next(COMMENT_META_DATA);
    //     this._opa.isShowCommentNavBar = true;
    // }

    formBuilderDataChanged(formEvent: any): any {
        switch (formEvent) {
            case 'CHANGED':
                return this.consultingService.isFormBuilderDataChangePresent = true;
            case 'SAVE_COMPLETE': {
                if (this.consultingService.isFormBuilderDataChangePresent) {
                    this.consultingService.triggerSaveComplete.next(true);
                }
                return this.consultingService.isFormBuilderDataChangePresent = false;
            }

            case 'ERROR':
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                break;
            default: break;
        }
    }

    private saveFormPersonEntity(personEntityId: number, entityNumber: number): void {
        this.$subscriptions.push(this._formService.saveFormPersonEntityDetails({
            disclosureId: this.consultingForm.consultingFormDisclosure.disclosureId,
            personEntityId: personEntityId,
            entityNumber: entityNumber
        }).subscribe((data) => {
            this.dataStore.updateStore(['consultingFormDisclosure'], { consultingFormDisclosure: data });
            this.canShowEntityFields = false;
            this.showTextAreaLimiter = false;
            this.consultingService.isDataChangeAvailableInEntity = false;
            this.resetEntityDefaultValue(this.entityDetailsAlreadySave.coiEntity.entityName);
            setTimeout(() => {
                this.isNewEntityFromSearch = false;
            });
        }));
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    backToSearch(): void {
        this.showTextAreaLimiter = false;
        this.canShowEntityFields = false;
        setTimeout(() => {
            this.isNewEntityFromSearch = false;
        });
    }

    phoneNumberValidation(input: any): void{ //common functions for validation
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

    inputRestriction(event: any): void {
        const pattern = /[0-9\+\-\/\ ]/;
        if (!pattern.test(String.fromCharCode(event.charCode))) {
            event.preventDefault();
        }
    }

    checkForSubmitDisable() {
        this.consultingService.canDisableSubmit = this.entityDetails.coiEntity.entityName ? false : true;
    }

 onEntityChanges() {
    this.consultingService.isDataChangeAvailableInEntity = true;
    this.checkForSubmitDisable();
 }

}
