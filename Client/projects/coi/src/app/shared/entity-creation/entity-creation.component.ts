import { Component, EventEmitter, Input, Output } from '@angular/core';
import { isValidEmailAddress, inputRestrictionForNumberField, phoneNumberValidation } from '../../common/utilities/custom-utilities';
import { interval, Subject, Subscription } from 'rxjs';
import { debounce } from 'rxjs/operators';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonService } from '../../common/services/common.service';
import { EntityCreationService } from './entity-creation.service';
import { getEndPointOptionsForCountry } from './../../../../../fibi/src/app/common/services/end-point.config';
import { isEmptyObject } from './../../../../../fibi/src/app/common/utilities/custom-utilities';
import { Country, Create_Entity, showEntityToast } from '../../entity-management-module/shared/entity-interface';
import { AutoSaveService } from '../../common/services/auto-save.service';
import { OverviewTabSection } from '../../entity-management-module/shared/entity-constants';

@Component({
  selector: 'app-entity-creation',
  templateUrl: './entity-creation.component.html',
  styleUrls: ['./entity-creation.component.scss'],
  providers: [EntityCreationService]
})
export class EntityCreationComponent {

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

    constructor(private _entityCreateService: EntityCreationService, private _router: Router,private _route: ActivatedRoute,
        private _commonService: CommonService, private _autoSaveService: AutoSaveService) {}

    ngOnInit() {
        this.countrySearchOptions = getEndPointOptionsForCountry();
        this.externalProccedSubscribe();
        this.triggerSingleSave();
        this.triggerExternalSave();
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

    externalProccedSubscribe() {
        this.initalProceed.subscribe((data) => {
            if(data) {
                this.entityMandatoryValidation();
                if(!this.mandatoryList.size) {
                    // this.emitEntityRO.emit(true);
                    this.saveEntity.next(true);
                }
            }
        })
    }

    triggerExternalSave() {
        this.saveEntity.subscribe((data) => {
            this.$subscriptions.push(this._entityCreateService.createEntity(this.createEntityObj).subscribe((data: any) => {
                this._router.navigate(['/coi/manage-entity/entity-overview'],
                    { queryParams: { entityManageId: data.entityId } }
                );
            }));
        });
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
        } else {
            this.createEntityObj.entityOwnershipTypeCode = null;
        }
    }

    changeEvent(key) {
        this.isFormDataChanged = true;
        if(this.saveMode == 'AUTO') {
            this.createEntityObj[key] = this.createEntityObj[key].trim();
            this.autoSaveRO[key] = this.createEntityObj[key];
            this.$debounceEvent.next(true);
        }
    }

    numberChangeEvent() {
        this.$debounceNumberEvent.next(true);
    }

    autoSaveAPI() {
        this.entityMandatoryValidation();
        if(!this.mandatoryList.size && this.isFormDataChanged) {
            this.autoSaveRO.entityId =  this._route.snapshot.queryParamMap.get('entityManageId');
            this._commonService.setLoaderRestriction();
            this.$subscriptions.push(this._entityCreateService.autoSaveService(this.autoSaveRO).subscribe((data: any) => {
                this.emitSaveObj.emit(this.autoSaveRO);
                this.autoSaveRO = {};
                showEntityToast('SUCCESS');
            }, err => {
                console.log(err);
                showEntityToast('ERROR');
            }
        ));
            this._commonService.removeLoaderRestriction();
        }
    }

    selectedCountryEvent(event: any): void {
        if(event) {
            this.createEntityObj.countryCode = event.countryCode;
            this.changeEvent('countryCode');
        } else {
            this.createEntityObj.countryCode = '';
        }
    }

    private entityMandatoryValidation(): void {
        this.clearValidation('primaryName');
        this.clearValidation('primaryAddressLine1');
        this.clearValidation('country');
        this.clearValidation('city');
        this.clearValidation('state');
        this.clearValidation('postCode');
        if (!this.createEntityObj.primaryName) {
            this.mandatoryList.set('primaryName', 'Please enter entity name.');
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
            this.$subscriptions.push(this._entityCreateService.validateDUNS(this.createEntityObj.dunsNumber).subscribe((data:any) => {
                if(data) {
                    this.mandatoryList.set('duns', 'An entity with this DUNS number already exists');
                } else {
                    this.changeEvent("dunsNumber");
                }
            }));
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
        }
    }

    clearValidation(type) {
        this.mandatoryList.delete(type);
    }


}
