import { Component } from '@angular/core';
import { AdditionalAddress } from '../../shared/entity-interface';
import { getEndPointOptionsForCountry } from 'projects/fibi/src/app/common/services/end-point.config';
import { deepCloneObject, hideModal, isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { EntityOverviewService } from '../entity-overview.service';
import { CommonService } from '../../../common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { COIModalConfig, ModalActionEvent } from '../../../shared-components/coi-modal/coi-modal.interface';
import { closeCommonModal, openCommonModal } from '../../../common/utilities/custom-utilities';

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
    isEditIndex: null | number = null;
    addressTypeDefaultValue = '';
    selectAddressType = null;
    selectedCountry = null;
    isSaving = false;
    deletePrimaryKey = null;
    CONFIRMATION_MODAL_ID = 'address-delete-confirm-modal';
    modalConfig = new COIModalConfig(this.CONFIRMATION_MODAL_ID , 'Delete', 'Cancel');

    constructor(private _entityOverviewService: EntityOverviewService, private _dataStorService: EntityDataStoreService, private _commonService: CommonService) {}
    ngOnInit() {
        this.countrySearchOptions = getEndPointOptionsForCountry();
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    selectedCountryEvent(event: any): void {
        if(event) {
            console.log(event);
            this.additionalAddressObj.countryCode = event.countryCode;
            this.selectedCountry = event;
        } else {
            this.additionalAddressObj.countryCode = '';
            this.selectedCountry = null;
        }
    }

    clearAdditionalAddress() {
        this.mandatoryList.clear();
        this.additionalAddressObj = new AdditionalAddress();
        this.countrySearchOptions = getEndPointOptionsForCountry();
        this.isEditIndex = null;
        this.addressTypeDefaultValue = '';
        this.selectAddressType = null;
        this.selectedCountry = null;
        this.selectedAddressType = [];
        this.deletePrimaryKey = null;
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
                newAddress.country = this.selectedCountry;
                newAddress.entityAddressType = this.selectAddressType;
                this.additionalAddresses.push(newAddress);
                this._dataStorService.updateStore(['entityMailingAddresses'], { 'entityMailingAddresses':  this.additionalAddresses });
                this.clearAdditionalAddress();
            }))
        }
    }

    onAddressTypeSelect(event) {
        if(event) {
            this.additionalAddressObj.addressTypeCode = event[0]?.code;
            this.selectAddressType = event[0];
        } else {
            this.additionalAddressObj.addressTypeCode = null;
            this.selectAddressType = null;
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
        if(!this.additionalAddressObj.addressTypeCode) {
            this.mandatoryList.set('addressTypeCode', 'Please select addressType.');
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


    confirmDelete(address, index: number) {
        this.isEditIndex = index;
        this.deletePrimaryKey = address.entityMailingAddressId;
        // logic to show the whole address.
        openCommonModal(this.CONFIRMATION_MODAL_ID);
    }

    editAddress(address, index: number) {
        console.log(address);
        this.isEditIndex = index;
        this.setAddressDetails(address);
        openModal('addAdditionalAddress');
    }

    setAddressDetails(address) {
        this.addressTypeDefaultValue = address.entityAddressType.description;
        this.additionalAddressObj.addressTypeCode = address.addressTypeCode;
        this.selectAddressType = address.entityAddressType;
        this.additionalAddressObj.entityId = address.entityId;
        this.additionalAddressObj.entityMailingAddressId = address.entityMailingAddressId;
        this.additionalAddressObj.addressLine1 = address.addressLine1;
        this.additionalAddressObj.addressLine2 = address.addressLine2;
        this.additionalAddressObj.countryCode = address.countryCode;
        this.selectedCountry = address.country;
        this.countrySearchOptions = getEndPointOptionsForCountry();
        this.countrySearchOptions.defaultValue = address.country.countryName;
        this.additionalAddressObj.city = address.city;
        this.additionalAddressObj.state = address.state;
        this.additionalAddressObj.postCode = address.postCode;
    }

    editIndustry() {
        if (!this.isSaving) {
            this.isSaving = true;
            this.entityMandatoryValidation();
            if (!this.mandatoryList.size) {
                this.$subscriptions.push(this._entityOverviewService.updateAdditionalAddresses(this.additionalAddressObj).subscribe((res: any) => {
                    this.additionalAddresses[this.isEditIndex].addressTypeCode = this.additionalAddressObj.addressTypeCode;
                    this.additionalAddresses[this.isEditIndex].addressLine1 = this.additionalAddressObj.addressLine1;
                    this.additionalAddresses[this.isEditIndex].addressLine2 = this.additionalAddressObj.addressLine2;
                    this.additionalAddresses[this.isEditIndex].countryCode = this.additionalAddressObj.countryCode;
                    this.additionalAddresses[this.isEditIndex].country = this.selectedCountry;
                    this.additionalAddresses[this.isEditIndex].entityAddressType = this.selectAddressType;
                    this.additionalAddresses[this.isEditIndex].city = this.additionalAddressObj.city;
                    this.additionalAddresses[this.isEditIndex].state = this.additionalAddressObj.state;
                    this.additionalAddresses[this.isEditIndex].postCode = this.additionalAddressObj.postCode;
                    // this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Registration Detail updated successfully.');
                    this.clearAdditionalAddress();
                    this.isSaving = false;
                }, err => {
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                    this.isSaving = false;
                }));
            }

        }
    }

    postConfirmation(modalAction: ModalActionEvent) {
        if(modalAction.action == 'PRIMARY_BTN') {
            this.deleteAddress();
        } else {
            this.clearAdditionalAddress();
        }
        closeCommonModal(this.CONFIRMATION_MODAL_ID);
    }

    deleteAddress() {
        if(!this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(this._entityOverviewService.deleteAdditionalAddress(this.deletePrimaryKey).subscribe((res: any) => {
                this.additionalAddresses.splice(this.isEditIndex, 1);
                // this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Additional address deleted successfully.');
                this.clearAdditionalAddress();
                this.isSaving = false;
            }, err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                this.isSaving = false;
            }));
        }
    }
}
