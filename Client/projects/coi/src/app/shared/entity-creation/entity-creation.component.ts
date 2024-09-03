import {Component, EventEmitter, Input, OnDestroy, OnInit, Output} from '@angular/core';
import { isValidEmailAddress, inputRestrictionForNumberField, phoneNumberValidation } from '../../common/utilities/custom-utilities';
import { interval, Subject, Subscription } from 'rxjs';
import { debounce } from 'rxjs/operators';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonService } from '../../common/services/common.service';
import { EntityCreationService } from './entity-creation.service';
import { getEndPointOptionsForCountry } from './../../../../../fibi/src/app/common/services/end-point.config';
import { deepCloneObject, hideModal, isEmptyObject, openModal } from './../../../../../fibi/src/app/common/utilities/custom-utilities';
import {
    Country,
    Create_Entity,
    EntityOwnerShip,
    removeToast,
    showEntityToast
} from '../../entity-management-module/shared/entity-interface';
import { AutoSaveService } from '../../common/services/auto-save.service';
import { OverviewTabSection } from '../../entity-management-module/shared/entity-constants';
import { EntityManagementService } from '../../entity-management-module/entity-management.service';
import { InformationAndHelpTextService } from '../../common/services/informationAndHelpText.service';
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
    autoSaveRO: any = {};
    @Input() isCreateScreen = false;
    @Input() saveMode: 'AUTO' | 'EXTERNAL' = 'EXTERNAL';
    @Input() saveEntity = new Subject();
    @Input() initalProceed = new Subject();
    @Input() createEntityObj: Create_Entity = new Create_Entity();
    @Input() countryDetails: Country = new Country();
    @Input() canNavigateToEntity: boolean = true;
    @Input() isEditMode = true;
    ownershipTypOptions = 'ENTITY_OWNERSHIP_TYPE#OWNERSHIP_TYPE_CODE#false#false';
    @Output() emitSaveObj = new EventEmitter<any>();
    @Output() emitEntityRO = new EventEmitter<any>();
    $debounceEvent = new Subject<any>();
    $debounceNumberEvent = new Subject<any>();
    changedKeys= [];
    isFormDataChanged = false;
    localDUNS: any;
    localUEI: any;
    localCAGE: any;
    selectedOwnerShipType: any = [];
    overViewTab = OverviewTabSection;
    @Input() manualAutoSaveEvent = new Subject();

    constructor(private _entityCreateService: EntityCreationService, private _router: Router,private _route: ActivatedRoute,
        private _commonService: CommonService, private _autoSaveService: AutoSaveService) {}

    ngOnInit() {
        this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
        this.isFormDataChanged = false;
        this.setCommonChangesFlag(false);
        this.externalProccedSubscribe();
        this.triggerSingleSave();
        this.triggerExternalSave();
        this.triggerNumberCheck();
        this.autoSaveSubscribe();
        this.subscribeManualAutoSaveEvent();
        this.setDefaultValues();
    }

    subscribeManualAutoSaveEvent() {
        this.$subscriptions.push(this.manualAutoSaveEvent.subscribe((event: any) => {
            if(event) {
                let createEntityObj = new Create_Entity();
                let test = this.createEntityObj.entityOwnerShip;
                createEntityObj.entityName = event?.orgDetails?.primaryName;
                createEntityObj.primaryAddressLine1 = event?.orgDetails?.primaryAddress?.streetAddress?.line1;
                createEntityObj.primaryAddressLine2 = event?.orgDetails?.primaryAddress?.streetAddress?.line2;
                createEntityObj.city = event?.orgDetails?.primaryAddress?.addressLocality?.name;
                createEntityObj.state = event?.orgDetails?.primaryAddress?.addressRegion?.abbreviatedName;
                createEntityObj.countryCode = event?.orgDetails?.primaryAddress?.addressCountry?.isoAlpha2Code;
                createEntityObj.postCode = event?.orgDetails?.primaryAddress?.postalCode;
                createEntityObj.dunsNumber = event?.orgDetails?.duns;
                createEntityObj.phoneNumber = event?.orgDetails?.telephone[0]?.telephoneNumber;
                createEntityObj.entityOwnershipTypeCode = this.createEntityObj.entityOwnershipTypeCode;
                createEntityObj.isDunsMatched = event?.isDunsMatched;
                this.createEntityObj = deepCloneObject(createEntityObj);
                delete this.createEntityObj['entityOwnerShip'];
                Object.keys(this.createEntityObj).forEach((ele) => {
                    this.changeEvent(ele);
                })
                this.createEntityObj.entityOwnerShip = test;
                this.createEntityObj.country ={'countryTwoCode':event?.orgDetails?.primaryAddress?.addressCountry?.isoAlpha2Code, 'countryName': event?.orgDetails?.primaryAddress?.addressCountry?.name}
            }
        }))
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

    externalProccedSubscribe() {
        this.$subscriptions.push(this.initalProceed.subscribe((data) => {
            if(data) {
                this.entityMandatoryValidation();
                if(!this.mandatoryList.size) {
                    this.emitEntityRO.emit(true);
                }
            }
        }));
    }

    triggerExternalSave() {
        this.$subscriptions.push(this.saveEntity.subscribe((data) => {
            this.entityMandatoryValidation();
            if (!this.mandatoryList.size) {
                delete this.createEntityObj['entityOwnerShip'];
                this.$subscriptions.push(this._entityCreateService.createEntity(this.createEntityObj).subscribe((data: any) => {
                    this.isFormDataChanged = false;
                    this.setCommonChangesFlag(false);
                    if(this.canNavigateToEntity) {
                        this._router.navigate(['/coi/manage-entity/entity-overview'],
                            { queryParams: { entityManageId: data.entityId } }
                        );
                    } else {
                        this.emitSaveObj.emit({
                            'entityId': data.entityId,
                            'entityName': this.createEntityObj.entityName
                    });
                    }
                }));
            }
        }));
    }

    triggerSingleSave() {
        this.$subscriptions.push(this.$debounceEvent.pipe(debounce(() => interval(2000))).subscribe((data: any) => {
          if (data) {
            this._autoSaveService.commonSaveTrigger$.next(true);
            // this.autoSaveAPI();
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
            if (!this.isCreateScreen) {
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
                this.emitSaveObj.emit({'autoSaveRO': this.autoSaveRO, 'isMandatoryFilled': true});
                this.localDUNS = this.createEntityObj.dunsNumber;
                this.localCAGE = this.createEntityObj.cageNumber;
                this.localUEI = this.createEntityObj.ueiNumber;                
                this.autoSaveRO = {};
                this.isFormDataChanged = false;
                this.setCommonChangesFlag(false);
                this.navigateToRoute();
                showEntityToast('SUCCESS');
            }, err => {
                showEntityToast('ERROR');
            }
        ));
            this._commonService.removeLoaderRestriction();
        } else if(this.mandatoryList.size){
            if(this._commonService.isNavigationStopped) {
                openModal('coi-entity-confirmation-modal');
            }
        }
    }

    leaveSlider(){
        removeToast('ERROR');
        removeToast('SUCCESS');
        this.setCommonChangesFlag(false);
        hideModal('coi-entity-confirmation-modal');
        this._router.navigateByUrl(this._commonService.attemptedPath);
    }

    setCommonChangesFlag(flag) {
        this._commonService.hasChangesAvailable = this.isCreateScreen ? false : flag;
    }

    navigateToRoute() {
        if(this._commonService.isNavigationStopped) {
            hideModal('coi-entity-confirmation-modal');
            this._router.navigateByUrl(this._commonService.attemptedPath);
        }
    }

    selectedCountryEvent(event: any): void {
        if(event) {
            this.createEntityObj.countryCode = event.countryCode;
            this.changeEvent('countryCode');
            if(!this.isCreateScreen) {
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
            this.mandatoryList.set('certifiedEmail', 'Please enter valid certifiedEmail.');
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
            this.$subscriptions.push(this._entityCreateService.validateDUNS(this.createEntityObj.dunsNumber).subscribe((data:any) => {
                if(data) {
                    this.mandatoryList.set('duns', 'An entity with this DUNS number already exists');
                } else {
                    this.changeEvent("dunsNumber");
                }
            }));
        } else {
            this.changeEvent("dunsNumber");
        }
    }

    checkForDuplicateUEI() {
        this.clearValidation('uei');
        if(this.createEntityObj.ueiNumber && this.createEntityObj.ueiNumber != this.localUEI) {
            this.$subscriptions.push(this._entityCreateService.validateUEI(this.createEntityObj.ueiNumber).subscribe((data:any) => {
                if(data) {
                    this.mandatoryList.set('uei', 'An entity with this UEI number already exists');
                } else {
                    this.changeEvent('ueiNumber');
                }
            }));
        } else {
            this.changeEvent("dunsNumber");
        }
    }

    checkForDuplicateCAGE() {
        this.clearValidation('cage');
        if(this.createEntityObj.cageNumber && this.createEntityObj.cageNumber != this.localCAGE) {
            this.$subscriptions.push(this._entityCreateService.validateCAGE(this.createEntityObj.cageNumber).subscribe((data:any) => {
                if(data) {
                    this.mandatoryList.set('cage', 'An entity with this CAGE number already exists');
                } else {
                    this.changeEvent('cageNumber');
                }
            }));
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
