import {Component, EventEmitter, Input, OnDestroy, OnInit, Output} from '@angular/core';
import { isValidEmailAddress, inputRestrictionForNumberField, phoneNumberValidation } from '../../common/utilities/custom-utilities';
import { interval, Subject, Subscription } from 'rxjs';
import { debounce } from 'rxjs/operators';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonService } from '../../common/services/common.service';
import { EntityCreationService } from './entity-creation.service';
import { getEndPointOptionsForCountry } from './../../../../../fibi/src/app/common/services/end-point.config';
import { hideModal, isEmptyObject, openModal } from './../../../../../fibi/src/app/common/utilities/custom-utilities';
import {
    Country,
    Create_Entity,
    DuplicateCheckObj,
    EntityOwnerShip,
    removeToast,
    showEntityToast
} from '../../entity-management-module/shared/entity-interface';
import { AutoSaveService } from '../../common/services/auto-save.service';
import { OverviewTabSection } from '../../entity-management-module/shared/entity-constants';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';


@Component({
  selector: 'app-entity-creation',
  templateUrl: './entity-creation.component.html',
  styleUrls: ['./entity-creation.component.scss'],
  providers: [EntityCreationService]
})
export class EntityCreationComponent implements OnInit, OnDestroy {

    clearCountryField: any;
    countrySearchOptions: any;
    mandatoryList = new Map();
    $subscriptions: Subscription[] = [];
    ownershipTypOptions = 'ENTITY_OWNERSHIP_TYPE#OWNERSHIP_TYPE_CODE#false#false';
    autoSaveRO: any = {};
    $debounceEvent = new Subject<any>();
    $debounceNumberEvent = new Subject<any>();
    changedKeys= [];
    isFormDataChanged = false;
    localDUNS: any;
    localUEI: any;
    localCAGE: any;
    selectedOwnerShipType: any = [];
    overViewTab = OverviewTabSection;
    @Input() isCreateView = false;
    @Input() saveMode: 'AUTO' | 'EXTERNAL' = 'EXTERNAL';
    @Input() createEntityObj = new Create_Entity();
    @Input() countryDetails = new Country();
    @Input() canNavigateToEntity = true;
    @Input() isEditMode = true;
    @Input() $performAction = new Subject<'SAVE_AND_VALIDATE'|'VALIDATE_ONLY'>();
    @Output() emitAutoSaveObj = new EventEmitter<any>();
    @Output() emitEntityDetails = new EventEmitter<any>();
    @Output() emitMandatoryResponse = new EventEmitter<DuplicateCheckObj>();

    constructor(private _entityCreateService: EntityCreationService, private _router: Router,private _route: ActivatedRoute,
        private _commonService: CommonService, private _autoSaveService: AutoSaveService) {}

    ngOnInit() {
        this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
        this.isFormDataChanged = false;
        this.setCommonChangesFlag(false);
        this.triggerSingleSave();
        this.listenManadatoryCheck();
        this.triggerNumberCheck();
        this.autoSaveSubscribe();
        this.setDefaultValues();
    }

    setDefaultValues() {
        if(!isEmptyObject(this.countryDetails)) {
            this.countrySearchOptions.defaultValue = this.countryDetails.countryName;
            this.clearCountryField = new String('false');
        }
        if(!isEmptyObject(this.createEntityObj)) {
            this.localDUNS = this.createEntityObj.dunsNumber;
            this.localCAGE = this.createEntityObj.cageNumber;
            this.localUEI = this.createEntityObj.ueiNumber;
            this.selectedOwnerShipType.push({
                'code': this.createEntityObj?.entityOwnerShip?.ownershipTypeCode,
                'description': this.createEntityObj?.entityOwnerShip?.description
            });
        }
    }

    listenManadatoryCheck() {
        this.$subscriptions.push(this.$performAction.subscribe((data: 'SAVE_AND_VALIDATE'|'VALIDATE_ONLY') => {
            this.entityMandatoryValidation();
            if(!this.mandatoryList.size && data === 'VALIDATE_ONLY') {
                const ENTITY_DETAILS = new DuplicateCheckObj();
                ENTITY_DETAILS.entityName = this.createEntityObj.entityName;
                ENTITY_DETAILS.countryCode = this.createEntityObj.countryCode;
                ENTITY_DETAILS.primaryAddressLine1 = this.createEntityObj.primaryAddressLine1;
                ENTITY_DETAILS.primaryAddressLine2 = this.createEntityObj.primaryAddressLine2;
                this.emitMandatoryResponse.emit(ENTITY_DETAILS);
            }
            if(data === 'SAVE_AND_VALIDATE' && !this.mandatoryList.size) {
                this.saveEntity();
            }
        }));
    }

    saveEntity() {
        delete this.createEntityObj['entityOwnerShip'];
        this.$subscriptions.push(this._entityCreateService.createEntity(this.createEntityObj).subscribe((data: any) => {
            this.isFormDataChanged = false;
            this.setCommonChangesFlag(false);
            if (this.canNavigateToEntity) {
                this._router.navigate(['/coi/manage-entity/entity-overview'],
                    { queryParams: { entityManageId: data.entityId } }
                );
            } else {
                this.emitEntityDetails.emit({
                    'entityId': data.entityId,
                    'entityName': this.createEntityObj.entityName
            });
            }
        }));
    }

    triggerSingleSave() {
        this.$subscriptions.push(this.$debounceEvent.pipe(debounce(() => interval(2000))).subscribe((data: any) => {
          if (data) {
            this._autoSaveService.commonSaveTrigger$.next(true);
          }
        }
        ));
      }

      autoSaveSubscribe() {
        this.$subscriptions.push(this._autoSaveService.autoSaveTrigger$.subscribe(event => this.autoSaveAPI()));
      }

      triggerNumberCheck() {
        this.$subscriptions.push(this.$debounceNumberEvent.pipe(debounce(() => interval(1000))).subscribe((data: any) => {
          if (data) {
            this.checkForDuplicateDUNS();
            this.checkForDuplicateCAGE();
            this.checkForDuplicateUEI();
          }
        }
        ));
      }

    onOwnerShipTypeSelect(event) {
        if(event && event.length) {
            this.createEntityObj.entityOwnershipTypeCode = event[0].code;
            this.changeEvent('entityOwnershipTypeCode');
            if (!this.isCreateView) {
                this.createEntityObj.entityOwnerShip = new EntityOwnerShip();
                this.createEntityObj.entityOwnerShip.ownershipTypeCode = event[0].code;
                this.createEntityObj.entityOwnerShip.description = event[0].description;
            }
        } else {
            this.createEntityObj.entityOwnershipTypeCode = null;
            this.createEntityObj.entityOwnerShip = new EntityOwnerShip();
            this.changeEvent('entityOwnershipTypeCode');
        }
    }

    changeEvent(key) {
        this.isFormDataChanged = true;
        this.setCommonChangesFlag(true);
        if(this.saveMode == 'AUTO') {
            if(typeof(this.createEntityObj[key]) == 'string') {
                this.createEntityObj[key] = this.createEntityObj[key].trim();
            }
            this.autoSaveRO[key] = this.createEntityObj[key];
            this.$debounceEvent.next(true);
        }
    }

    numberChangeEvent() {
        this.setCommonChangesFlag(true);
        this.$debounceNumberEvent.next(true);
    }

    autoSaveAPI() {
        this.entityMandatoryValidation();
        if(!this.mandatoryList.size && this.isFormDataChanged) {
            this.autoSaveRO.entityId =  this._route.snapshot.queryParamMap.get('entityManageId');
            this._commonService.setLoaderRestriction();
            this.$subscriptions.push(this._entityCreateService.autoSaveService(this.autoSaveRO).subscribe((data: any) => {
                if(this.autoSaveRO.hasOwnProperty('entityOwnershipTypeCode')) {
                    this.autoSaveRO['entityOwnershipType'] = this.createEntityObj['entityOwnerShip'];
                }
                if(this.autoSaveRO.hasOwnProperty('countryCode')) {
                    this.autoSaveRO['country'] = this.createEntityObj['country'];
                }
                this.emitAutoSaveObj.emit({'autoSaveRO': this.autoSaveRO});
                this.localDUNS = this.createEntityObj.dunsNumber;
                this.localCAGE = this.createEntityObj.cageNumber;
                this.localUEI = this.createEntityObj.ueiNumber;
                this.autoSaveRO = {};
                this.isFormDataChanged = false;
                showEntityToast('SUCCESS');
                this.setCommonChangesFlag(false);
            }, err => {
                showEntityToast('ERROR');
            }
        ));
            this._commonService.removeLoaderRestriction();
        }
    }

    setCommonChangesFlag(flag) {
        this._commonService.setChangesAvailable(this.isCreateView ? false : flag);
    }

    selectedCountryEvent(event: any): void {
        if(event) {
            this.createEntityObj.countryCode = event.countryCode;
            this.changeEvent('countryCode');
            if(!this.isCreateView) {
                this.createEntityObj.country = event;
            }
        } else {
            this.createEntityObj.countryCode = '';
        }
    }

    private entityMandatoryValidation(): void {
        this.clearValidation('entityName');
        this.clearValidation('primaryAddressLine1');
        this.clearValidation('country');
        this.clearValidation('city');
        this.clearValidation('state');
        this.clearValidation('postCode');
        this.clearValidation('ownershipType');
        if (!this.createEntityObj.entityName) {
            this.mandatoryList.set('entityName', 'Please enter entity name.');
        }
        if (!this.createEntityObj.primaryAddressLine1) {
            this.mandatoryList.set('primaryAddressLine1', 'Please enter address 1.');
        }
        if (!this.createEntityObj.countryCode) {
            this.mandatoryList.set('country', 'Please select a country.');
        }
        if (!this.createEntityObj.city) {
            this.mandatoryList.set('city', 'Please enter city.');
        }
        if (!this.createEntityObj.state) {
            this.mandatoryList.set('state', 'Please enter state.');
        }
        if (!this.createEntityObj.postCode) {
            this.mandatoryList.set('postCode', 'Please enter postal code.');
        }
        if (!this.createEntityObj.entityOwnershipTypeCode) {
            this.mandatoryList.set('ownershipType', 'Please select ownership type.');
        }
    }

    checkForValidPhoneNumber(event) {
        if(inputRestrictionForNumberField(String.fromCharCode(event.charCode))) {
            event.preventDefault();
        }
    }

    validateEmail() {
        this.clearValidation('certifiedEmail');
        if(this.createEntityObj.certifiedEmail && !isValidEmailAddress(this.createEntityObj.certifiedEmail)) {
            this.mandatoryList.set('certifiedEmail', 'Please enter valid email address.');
        } else {
            this.changeEvent('certifiedEmail');
        }
    }

    validateAndAddPhoneNumber() {
        this.clearValidation('phone');
        if(this.createEntityObj.phoneNumber && phoneNumberValidation(this.createEntityObj.phoneNumber)) {
            this.mandatoryList.set('phone', 'Please enter valid phone.');
        } else {
            this.changeEvent('phoneNumber');
        }
    }

    checkForDuplicateDUNS() {
        this.clearValidation('duns');
        if(this.createEntityObj.dunsNumber && this.createEntityObj.dunsNumber != this.localDUNS) {
            this.createEntityObj.isDunsMatched = false;
            this.changeEvent("isDunsMatched");
            this._commonService.setLoaderRestriction();
            this.$subscriptions.push(this._entityCreateService.validateDUNS(this.createEntityObj.dunsNumber).subscribe((data:any) => {
                if(data) {
                    this.mandatoryList.set('duns', 'An entity with this DUNS number already exists');
                } else {
                    this.changeEvent("dunsNumber");
                }
            }));
            this._commonService.removeLoaderRestriction();
        } else {
            this.changeEvent("dunsNumber");
        }
    }

    checkForDuplicateUEI() {
        this.clearValidation('uei');
        if(this.createEntityObj.ueiNumber && this.createEntityObj.ueiNumber != this.localUEI) {
            this._commonService.setLoaderRestriction();
            this.$subscriptions.push(this._entityCreateService.validateUEI(this.createEntityObj.ueiNumber).subscribe((data:any) => {
                if(data) {
                    this.mandatoryList.set('uei', 'An entity with this UEI number already exists');
                } else {
                    this.changeEvent('ueiNumber');
                }
            }));
            this._commonService.removeLoaderRestriction();
        } else {
            this.changeEvent("dunsNumber");
        }
    }

    checkForDuplicateCAGE() {
        this.clearValidation('cage');
        if(this.createEntityObj.cageNumber && this.createEntityObj.cageNumber != this.localCAGE) {
            this._commonService.setLoaderRestriction();
            this.$subscriptions.push(this._entityCreateService.validateCAGE(this.createEntityObj.cageNumber).subscribe((data:any) => {
                if(data) {
                    this.mandatoryList.set('cage', 'An entity with this CAGE number already exists');
                } else {
                    this.changeEvent('cageNumber');
                }
            }));
            this._commonService.removeLoaderRestriction();
        } else {
            this.changeEvent("dunsNumber");
        }
    }

    clearValidation(type) {
        this.mandatoryList.delete(type);
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

}
