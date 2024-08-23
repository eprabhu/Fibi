import { Component } from '@angular/core';
import { AdditionalAddress } from '../../shared/entity-interface';
import { getEndPointOptionsForCountry } from 'projects/fibi/src/app/common/services/end-point.config';
import { deepCloneObject, hideModal, isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { EntityOverviewService } from '../entity-overview.service';

@Component({
  selector: 'app-additional-addresses',
  templateUrl: './additional-addresses.component.html',
  styleUrls: ['./additional-addresses.component.scss']
})
export class AdditionalAddressesComponent {
    additionalAddressObj: AdditionalAddress = new AdditionalAddress();
    clearCountryField = new String('true');
    countrySearchOptions: any;
    mandatoryList = new Map();
    $subscriptions: Subscription[] = [];
    entityId: any;
    additionalAddresses: any = [];
    addressTypOptions = 'ENTITY_ADDRESS_TYPE#ADDRESS_TYPE_CODE#false#false';
    selectedAddressType = [];

    constructor(private _entityOverviewService: EntityOverviewService, private _dataStorService: EntityDataStoreService) {}
    ngOnInit() {
        this.countrySearchOptions = getEndPointOptionsForCountry();
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    selectedCountryEvent(event: any): void {
        if(event) {
            this.additionalAddressObj.countryCode = event.countryCode;
        } else {
            this.additionalAddressObj.countryCode = '';
        }
    }

    clearAdditionalAddress() {
        this.mandatoryList.clear();
        this.additionalAddressObj = new AdditionalAddress();
        hideModal('addAdditionalAddress');
    }

    addIndustry() {
        this.entityMandatoryValidation();
        if(!this.mandatoryList.size) {
            this.additionalAddressObj.entityId = this.entityId;
            this.$subscriptions.push(this._entityOverviewService.addAdditionalAddress(this.additionalAddressObj).subscribe((data: any) => {
                console.log(data);
                let newAddress;
                newAddress = deepCloneObject(this.additionalAddressObj);
                newAddress.entityMailingAddressId = data.entityMailingAddressId;
                this.additionalAddresses.push(newAddress);
                this._dataStorService.updateStore(['entityMailingAddresses'], { 'entityMailingAddresses':  this.additionalAddresses });
                this.clearAdditionalAddress();
            }))
        }
    }

    onAddressTypeSelect(event) {
        if(event) {
            this.additionalAddressObj.addressType = event[0]?.code;
        } else {
            this.additionalAddressObj.addressType = null;
        }
    }

    addIndustryDetails() {
        openModal('addAdditionalAddress');
    }

    changeEvent(key) {

    }

    entityMandatoryValidation(): void {
        this.mandatoryList.clear();
        if(!this.additionalAddressObj.addressLine1) {
            this.mandatoryList.set('addressLine1', 'Please enter addressLine1.');
        }
        if(!this.additionalAddressObj.addressType) {
            this.mandatoryList.set('addressType', 'Please select addressType.');
        }
        if(!this.additionalAddressObj.city) {
            this.mandatoryList.set('city', 'Please enter city.');
        }
        if(!this.additionalAddressObj.countryCode) {
            this.mandatoryList.set('countryCode', 'Please select country.');
        }
        if(!this.additionalAddressObj.state) {
            this.mandatoryList.set('state', 'Please enter state.');
        }
        if(!this.additionalAddressObj.postCode) {
            this.mandatoryList.set('postCode', 'Please enter postCode.');
        }
    }

    private getDataFromStore() {
        const entityData = this._dataStorService.getData();
        if (isEmptyObject(entityData)) { return; }
        this.entityId = entityData?.entityDetails?.entityId;
        this.additionalAddresses = entityData.entityMailingAddresses;
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStorService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    getEntityFullAddress(entityOtherAddress) {
        let address = entityOtherAddress?.addressLine1;
        if (entityOtherAddress?.addressLine2) {
            address = address + ' , ' + entityOtherAddress?.addressLine2;
        }
        if(entityOtherAddress?.city) {
            address = address + ' , ' + entityOtherAddress?.city;
        }
        if(entityOtherAddress?.state) {
            address = address + ' , ' + entityOtherAddress?.state;
        }
        if(entityOtherAddress?.country?.countryName) {
            address = address + ' , ' + entityOtherAddress?.country?.countryName;
        }
        if(entityOtherAddress?.postCode) {
            address = address + ' , ' + entityOtherAddress?.postCode;
        }
        return address;
    }

}
