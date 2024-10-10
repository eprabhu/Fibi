import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { isValidEmailAddress, inputRestrictionForNumberField, phoneNumberValidation, openModal } from '../../common/utilities/custom-utilities';
import { interval, Subject, Subscription } from 'rxjs';
import { debounce } from 'rxjs/operators';
import { Router } from '@angular/router';
import { CommonService } from '../../common/services/common.service';
import { EntityCreationService } from './entity-creation.service';
import { getEndPointOptionsForCountry } from './../../../../../fibi/src/app/common/services/end-point.config';
import { deepCloneObject, isEmptyObject } from './../../../../../fibi/src/app/common/utilities/custom-utilities';
import {
    DuplicateCheckObj,
    EntityDetails,
    EntityFields,
    EntityNumberFields,
    EntityOwnerShip,
    EntityRequestFields,
    EntityUpdateClass,
    LookUpClass,
    removeToast,
    showEntityToast
} from '../../entity-management-module/shared/entity-interface';
import { AutoSaveService } from '../../common/services/auto-save.service';
import { AUTO_SAVE_DEBOUNCE_TIME, ENTITY_MANDATORY_FIELDS } from '../../entity-management-module/shared/entity-constants';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';

@Component({
    selector: 'app-entity-creation',
    templateUrl: './entity-creation.component.html',
    styleUrls: ['./entity-creation.component.scss'],
    providers: [EntityCreationService]
})
export class EntityCreationComponent implements OnInit, OnDestroy {

    clearCountryField: any;
    mandatoryList = new Map();
    $subscriptions: Subscription[] = [];
    OWNERSHIP_TYPE_OPTIONS = 'ENTITY_OWNERSHIP_TYPE#OWNERSHIP_TYPE_CODE#false#false';
    COUNTRY_SEARCH_OPTIONS: any;
    $debounceEvent = new Subject<string>();
    $debounceEventForNumber = new Subject<EntityNumberFields>();
    selectedOwnerShipType: Array<LookUpClass> = [];
    entityUpdateObj = new EntityUpdateClass();
    changeDetectionObj = new EntityFields();

    @Input() isCreateView = false;
    @Input() canNavigateToEntity = true;
    @Input() isEditMode = true;
    @Input() $performAction = new Subject<'SAVE_AND_VALIDATE' | 'VALIDATE_ONLY'>();
    @Input() entityDetails = new EntityDetails();
    @Output() emitAutoSaveObj = new EventEmitter<any>();
    @Output() emitEntityDetails = new EventEmitter<any>();
    @Output() emitMandatoryResponse = new EventEmitter<DuplicateCheckObj>();

    constructor(private _entityCreateService: EntityCreationService, private _router: Router,
        private _commonService: CommonService, private _autoSaveService: AutoSaveService) { }

    ngOnInit() {
        this.COUNTRY_SEARCH_OPTIONS = getEndPointOptionsForCountry(this._commonService.fibiUrl);
        this.setLocalEntityUpdateObj();
        this.setCommonChangesFlag();
        this.listenDebounceEvent();
        this.autoSaveSubscribe();
        this.listenNumberDebounceEvent();
        this.listenManadatoryCheck();
    }

    private setLocalEntityUpdateObj(): void {
        if (!isEmptyObject(this.entityDetails)) {
            Object.keys(this.entityDetails).forEach((ele: string) => {
                this.entityUpdateObj.entityRequestFields[ele] = this.entityDetails[ele];
            });
            this.COUNTRY_SEARCH_OPTIONS.defaultValue = this.entityDetails?.country?.countryName;
            this.clearCountryField = new String('false');
            const OWNERSHIP_OBJ = this.entityDetails?.entityOwnershipType;
            this.selectedOwnerShipType.push({ code: OWNERSHIP_OBJ?.ownershipTypeCode, description: OWNERSHIP_OBJ?.description });
        }
    }

    private autoSaveSubscribe(): void {
        this.$subscriptions.push(this._autoSaveService.autoSaveTrigger$.subscribe(event => this.checkMandatoryAndSave()));
    }

    private listenNumberDebounceEvent(): void {
        this.$subscriptions.push(this.$debounceEventForNumber.pipe(debounce(() => interval(500))).subscribe((data: EntityNumberFields) => {
            if (data) {
                this.numberChangeEvent(data);
            }
        }
        ));
    }

    private listenDebounceEvent(): void {
        this.$subscriptions.push(this.$debounceEvent.pipe(debounce(() => interval(AUTO_SAVE_DEBOUNCE_TIME))).subscribe((data: any) => {
            if (data) {
                this._autoSaveService.commonSaveTrigger$.next(true);
            }
        }
        ));
    }

    private listenManadatoryCheck(): void {
        this.$subscriptions.push(this.$performAction.subscribe((data: 'SAVE_AND_VALIDATE' | 'VALIDATE_ONLY') => {
            this.entityMandatoryValidation();
            if (!this.mandatoryList.size) {
                data === 'VALIDATE_ONLY' ? this.emitMandatoryCheckResponse() : this.saveEntity();
            }
        }));
    }

    private emitMandatoryCheckResponse(): void {
        const ENTITY_DETAILS = new DuplicateCheckObj();
        ENTITY_DETAILS.entityName = this.entityUpdateObj.entityRequestFields.entityName;
        ENTITY_DETAILS.countryCode = this.entityUpdateObj.entityRequestFields.countryCode;
        ENTITY_DETAILS.primaryAddressLine1 = this.entityUpdateObj.entityRequestFields.primaryAddressLine1;
        ENTITY_DETAILS.primaryAddressLine2 = this.entityUpdateObj.entityRequestFields.primaryAddressLine2;
        this.emitMandatoryResponse.emit(ENTITY_DETAILS);
    }

    private saveEntity(): void {
        this.entityUpdateObj.entityId = null;
        const MODIFIED_PAYLOAD = this.getModifiedPayload(this.entityUpdateObj);
        this.$subscriptions.push(this._entityCreateService.createEntity(MODIFIED_PAYLOAD).subscribe((data: any) => {
            this.setCommonChangesFlag();
            if (this.canNavigateToEntity) {
                this._router.navigate(['/coi/manage-entity/entity-overview'], { queryParams: { entityManageId: data.entityId } });
            } else {
                this.emitEntityDetails.emit({
                    entityId: data.entityId,
                    entityName: MODIFIED_PAYLOAD.entityRequestFields.entityName
                });
            }
        }));
    }

    private numberChangeEvent(type: EntityNumberFields): void {
        switch (type) {
            case 'ueiNumber':
                this.checkForDuplicate(type, 'validateUEI'); //one time and api name assign in variable
                break;
            case 'dunsNumber':
                this.checkForDuplicate(type, 'validateDUNS');
                break;
            case 'cageNumber':
                this.checkForDuplicate(type, 'validateCAGE');
                break;
            default:
                break;
        }
    }

    private autoSaveAPI(): void {
        const AUTO_SAVE_RO: EntityUpdateClass = this.getModifiedPayload( this.getAutoSaveRO());
        if (isEmptyObject(AUTO_SAVE_RO) || isEmptyObject(AUTO_SAVE_RO.entityRequestFields) || !Object.keys(AUTO_SAVE_RO.entityRequestFields).length) {
            return;
        }
        this.setupBeforeAPICall(AUTO_SAVE_RO);
        this._commonService.setLoaderRestriction();
        this.$subscriptions.push(this._entityCreateService.autoSaveService(AUTO_SAVE_RO).subscribe((data: any) => {
            this.emitStoreDataUpdateDetails(AUTO_SAVE_RO.entityRequestFields);
            this.stopAutoSaveLoader('SUCCESS');
            this.setCommonChangesFlag();
        }, err => {
            this.setChangesObject(AUTO_SAVE_RO.entityRequestFields, true);
            this.stopAutoSaveLoader('ERROR');
        }
        ));
        this._commonService.removeLoaderRestriction();
    }

    private checkMandatoryAndSave(): void {
        this.entityMandatoryValidation();
        const MANDATORY_NOT_FILLED = ENTITY_MANDATORY_FIELDS.some((ele: string) => this.mandatoryList.has(ele));
        if (MANDATORY_NOT_FILLED) {
            return;
        }
        this.autoSaveAPI();
    }

    private emitStoreDataUpdateDetails(updatedFields: EntityRequestFields): void {
        if(this.entityUpdateObj.entityRequestFields.entityOwnershipTypeCode) {
            updatedFields.entityOwnershipType = this.entityUpdateObj.entityRequestFields.entityOwnershipType;
        }
        if(this.entityUpdateObj.entityRequestFields.countryCode) {
            updatedFields.country = this.entityUpdateObj.entityRequestFields.country;
        }
        this.emitAutoSaveObj.emit({ 'autoSaveRO': updatedFields });
    }

    private getModifiedPayload(entityRequestFields: EntityUpdateClass): EntityUpdateClass {
        const MODIFIED_PAYLOAD: EntityUpdateClass = { ...entityRequestFields };
        delete MODIFIED_PAYLOAD.entityRequestFields?.country;
        delete MODIFIED_PAYLOAD.entityRequestFields?.entityOwnershipType;
        return MODIFIED_PAYLOAD;
    }

    private setChangesObject(autoSaveReqObj: EntityRequestFields, isChangesAvailable: boolean): void {
        Object.keys(autoSaveReqObj).forEach((ele) => {
            if (!this.mandatoryList.has(ele)) {
                this.changeDetectionObj[ele] = isChangesAvailable;
            }
        });
    }

    private getAutoSaveRO(): EntityUpdateClass {
        const AUTO_SAVE_PAYLOAD = new EntityUpdateClass();
        AUTO_SAVE_PAYLOAD.entityId = this.entityDetails.entityId;
        Object.keys(this.changeDetectionObj).forEach((ele) => {
            if (this.changeDetectionObj[ele] && !this.mandatoryList.has(ele)) {
                const VALUE = this.entityUpdateObj.entityRequestFields[ele];
                AUTO_SAVE_PAYLOAD.entityRequestFields[ele] = typeof VALUE === 'string' ? (VALUE?.trim() || null) : VALUE;
            }
        });
        return AUTO_SAVE_PAYLOAD;
    }

    private stopAutoSaveLoader(toastType: 'SUCCESS' | 'ERROR'): void {
        setTimeout(() => {
            if (!this._commonService.loaderRestrictedUrls.length) {
                this._commonService.autoSaveSavingLoader = 'HIDE';
                showEntityToast(toastType);
            }
        });
    }

    private setupBeforeAPICall(AUTO_SAVE_RO: EntityUpdateClass): void {
        removeToast('SUCCESS');
        removeToast('ERROR');
        this._commonService.autoSaveSavingLoader = 'SHOW';
        this.setChangesObject(AUTO_SAVE_RO.entityRequestFields, false);
    }

    private setCommonChangesFlag(): void {
        const HAS_TRUE_VALUE = Object.values(this.changeDetectionObj).some(value => value);
        this._commonService.setChangesAvailable(this.isCreateView ? false : HAS_TRUE_VALUE);
    }

    private checkForDuplicate(type: EntityNumberFields, apiName: keyof typeof this._entityCreateService): void {
        this.clearValidation(type);
        const CURRENT_VAL = this.entityUpdateObj.entityRequestFields[type]?.trim() || null;
        const SAVED_VAL = this.entityDetails?.[type];
        if (CURRENT_VAL === SAVED_VAL) {
            return;
        }
        this.changeDetectionObj[type] = true;
        this.setCommonChangesFlag();
        CURRENT_VAL ? this.triggerDuplicateCheckAPICall(apiName, CURRENT_VAL, type) : this.saveNumber(type);
    }

    private triggerDuplicateCheckAPICall(apiName: keyof typeof this._entityCreateService, CURRENT_VAL: string|null, type: EntityNumberFields ) {
        this._commonService.setLoaderRestriction();
        this.$subscriptions.push(this._entityCreateService[apiName](CURRENT_VAL).subscribe((data: any) => {
            data ? this.handleDuplicateNumber(type) : this.saveNumber(type);
        }));
        this._commonService.removeLoaderRestriction();
    }

    private saveNumber(type: EntityNumberFields): void {
        if (type === 'dunsNumber') {
            this.entityUpdateObj.entityRequestFields.isDunsMatched = false;
            this.changeDetectionObj['isDunsMatched'] = true;
        }
        this.changeEvent(type);
    }

    private handleDuplicateNumber(type: EntityNumberFields): void {
        const NUM_TYPE = type === 'dunsNumber' ? 'DUNS' : type === 'cageNumber' ? 'CAGE' : 'UEI';
        this.mandatoryList.set(type, `An entity with this ${NUM_TYPE} number already exists`);
        if (this._commonService.hasChangesAvailable && this._commonService.isNavigationStopped) {
            this._commonService.isShowLoader.next(false);
            openModal('coi-entity-confirmation-modal');
        }
    }

    private entityMandatoryValidation(): void {
        ENTITY_MANDATORY_FIELDS.forEach((field: string) => {
            this.clearValidation(field);
            if(typeof this.entityUpdateObj.entityRequestFields[field] === 'string') {
                this.entityUpdateObj.entityRequestFields[field] = this.entityUpdateObj?.entityRequestFields[field]?.trim();
            }
            if (!this.entityUpdateObj.entityRequestFields[field]) {
                let message = '';
                switch (field) {
                    case 'entityName':
                        message = 'Please enter the entity name.';
                        break;
                    case 'primaryAddressLine1':
                        message = 'Please enter the address line 1.';
                        break;
                    case 'countryCode':
                        message = 'Please select a country.';
                        break;
                    case 'postCode':
                        message = 'Please enter the postal code.';
                        break;
                    case 'entityOwnershipTypeCode':
                        message = 'Please select an ownership type.';
                        break;
                    default:
                        message = `Please enter the ${field}.`;
                }
                this.mandatoryList.set(field, message);
            }
        });
    }

    private clearValidation(type): void {
        this.mandatoryList.delete(type);
    }

    onOwnerShipTypeSelect(event: any): void {
        this.entityUpdateObj.entityRequestFields.entityOwnershipTypeCode = event?.[0]?.code || null;
        if (!this.isCreateView) {
            this.entityUpdateObj.entityRequestFields.entityOwnershipType = new EntityOwnerShip();
            this.entityUpdateObj.entityRequestFields.entityOwnershipType.ownershipTypeCode = event?.[0]?.code;
            this.entityUpdateObj.entityRequestFields.entityOwnershipType.description = event?.[0]?.description;
        }
        this.changeEvent('entityOwnershipTypeCode');
    }

    changeEvent(key: string): void {
        this.changeDetectionObj[key] = true;
        this.setCommonChangesFlag();
        this.$debounceEvent.next(key);
    }

    checkForValidPhoneNumber(event: any): void {
        if (inputRestrictionForNumberField(String.fromCharCode(event.charCode))) {
            event.preventDefault();
        }
    }

    validateEmail(): void {
        this.clearValidation('certifiedEmail');
        this.changeDetectionObj['certifiedEmail'] = true;
        this.setCommonChangesFlag();
        if (this.entityUpdateObj?.entityRequestFields?.certifiedEmail && !isValidEmailAddress(this.entityUpdateObj?.entityRequestFields?.certifiedEmail)) {
            this.mandatoryList.set('certifiedEmail', 'Please enter valid email address.');
        } else if (this.entityUpdateObj?.entityRequestFields?.certifiedEmail != this.entityDetails.certifiedEmail) {
            this.$debounceEvent.next('certifiedEmail');
        }
    }

    validateAndAddPhoneNumber(): void {
        this.clearValidation('phoneNumber');
        this.changeDetectionObj['phoneNumber'] = true;
        this.setCommonChangesFlag();
        if (this.entityUpdateObj?.entityRequestFields?.phoneNumber && phoneNumberValidation(this.entityUpdateObj?.entityRequestFields?.phoneNumber)) {
            this.mandatoryList.set('phoneNumber', 'Please enter valid phone.');
        } else if (this.entityUpdateObj?.entityRequestFields?.phoneNumber != this.entityDetails.phoneNumber) {
            this.$debounceEvent.next('phoneNumber');
        }
    }

    validateAndSaveNumber(key: EntityNumberFields) {
        this.$debounceEventForNumber.next(key);
    }

    selectedCountryEvent(event: any): void {
        this.entityUpdateObj.entityRequestFields.countryCode = event?.countryCode || null;
        if (!this.isCreateView && event) {
            this.entityUpdateObj.entityRequestFields.country = event;
        }
        this.changeEvent('countryCode');
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

}
