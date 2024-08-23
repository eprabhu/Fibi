
import { Component } from '@angular/core';
import { RegistrationDetails } from '../../shared/entity-interface';
import { hideModal, isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { EntityOverviewService } from '../entity-overview.service';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { CommonService } from '../../../common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { COIModalConfig, ModalActionEvent } from '../../../shared-components/coi-modal/coi-modal.interface';
import { closeCommonModal, openCommonModal } from '../../../common/utilities/custom-utilities';

@Component({
  selector: 'app-registration-details',
  templateUrl: './registration-details.component.html',
  styleUrls: ['./registration-details.component.scss']
})
export class RegistrationDetailsComponent {

    registrationDetails: RegistrationDetails = new RegistrationDetails();
    entityRegistrationTypeOption = 'ENTITY_REGISTRATION_TYPE#REG_TYPE_CODE#false#false';
    mandatoryList = new Map();
    $subscriptions: Subscription[] = [];
    entityId: any;
    entityRegistrations: any;
    selectedType: any;
    selectedRegistrationType = [];
    isEditIndex: null | number = null;
    deleteEntityRegistrationId = null;
    isSaving = false;
    entityRegistrationDefaultValue = '';
    modalConfig = new COIModalConfig('register-delete-confirm-modal', 'Delete', 'Cancel');

    constructor(private _entityOverviewService: EntityOverviewService, private _dataStoreService: EntityDataStoreService, private _commonService: CommonService) {}

    addRegistrationDetails(event) {
        if(event) {
            openModal('addRegistrationDetails');
        }
    }

    ngOnInit(){
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private getDataFromStore() {
        const entityData = this._dataStoreService.getData();
        if (isEmptyObject(entityData)) { return; }
        this.entityId = entityData?.entityDetails?.entityId;
        this.entityRegistrations = entityData.entityRegistrations;
    }

    clearRegistrationDetails() {
        this.registrationDetails = new RegistrationDetails();
        this.mandatoryList.clear();
        this.selectedType = '';
        this.selectedRegistrationType = [];
        this.entityRegistrationDefaultValue = '';
        this.isEditIndex = null;
        this.deleteEntityRegistrationId = null;
        hideModal('addRegistrationDetails');
    }

    addRegistration() {
        this.entityMandatoryValidation();
        if(!this.mandatoryList.size) {
            this.registrationDetails.entityId = this.entityId;
            this.$subscriptions.push(this._entityOverviewService.addRegistrationDetails(this.registrationDetails).subscribe((data: any) => {
                if(data) {
                    let registration: any = {};
                    registration.entityRegistrationId = data.entityRegistrationId;
                    registration.registrationTypeDescription = this.selectedType[0]?.description;
                    registration.regNumber = this.registrationDetails.regNumber;
                    this.entityRegistrations.push(registration);
                    this._dataStoreService.updateStore(['entityRegistrations'], { 'entityRegistrations':  this.entityRegistrations });
                }
                this.clearRegistrationDetails();
            }))
        }
    }

    entityRegistrationTypeSelect(event) {
        if(event) {
            this.registrationDetails.regTypeCode = event[0]?.code;
            this.selectedType = event;
        } else {
            this.registrationDetails.regTypeCode = null;
            this.selectedType = null;
        }
    }
    entityMandatoryValidation(): void {
        this.mandatoryList.clear();
        if(!this.registrationDetails.regTypeCode) {
            this.mandatoryList.set('regTypeCode', 'Please select registration type.');
        }
        if(!this.registrationDetails.regNumber) {
            this.mandatoryList.set('registrationNumber', 'Please enter registration number.');
        }
    }

    editRelationship(registration: any, index: number) {
        this.isEditIndex = index;
        this.setRegistrationDetails(registration);
                openModal('addRegistrationDetails');
    }

    setRegistrationDetails(registration) {
        this.registrationDetails.entityId = this.entityId;
        this.registrationDetails.regTypeCode = registration.regTypeCode;
        this.registrationDetails.entityRegistrationId = registration.entityRegistrationId;
        this.registrationDetails.regNumber = registration.regNumber;
        this.entityRegistrationDefaultValue = registration.registrationType ? registration.registrationType.description : registration.registrationTypeDescription;
    }

    editRegistration() {
        if (!this.isSaving) {
            this.isSaving = true;
            this.entityMandatoryValidation();
            if (!this.mandatoryList.size) {
                this.registrationDetails.entityId = this.entityId;
                this.$subscriptions.push(this._entityOverviewService.updateRegistrationDetails(this.registrationDetails).subscribe((data: any) => {
                    if (data) {
                        this.entityRegistrations[this.isEditIndex].registrationTypeDescription = this.selectedType && this.selectedType[0] ? this.selectedType[0]?.description : this.entityRegistrationDefaultValue;
                        this.entityRegistrations[this.isEditIndex].regNumber = this.registrationDetails.regNumber;
                        this.entityRegistrations[this.isEditIndex].regTypeCode = this.registrationDetails.regTypeCode;
                        this.entityRegistrations[this.isEditIndex].registrationType = null;
                        this._dataStoreService.updateStore(['entityRegistrations'], { 'entityRegistrations': this.entityRegistrations });
                    }
                    this.clearRegistrationDetails();
                    // this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Registration Details updated successfully.');
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
            this.deleteRegistration(this.deleteEntityRegistrationId);
        } else {
            this.clearRegistrationDetails();
        }
        closeCommonModal('register-delete-confirm-modal');
    }

    confirmDelete(registration, index: number) {
        this.isEditIndex = index;
        this.deleteEntityRegistrationId = registration.entityRegistrationId;
        this.entityRegistrationDefaultValue = registration.registrationType ? registration.registrationType.description : registration.registrationTypeDescription;
        openCommonModal('register-delete-confirm-modal');
    }

    deleteRegistration(entityRegistrationId) {
        if (!this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(this._entityOverviewService.deleteRegistrationDetails(entityRegistrationId).subscribe((data: any) => {
                if(data) {
                    this.entityRegistrations.splice(this.isEditIndex, 1);
                    this.clearRegistrationDetails();
                    this.isSaving = false;
                }
            }, err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                this.isSaving = false;
            }));
        }
    }

}
