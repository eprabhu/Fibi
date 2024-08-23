import { Component } from '@angular/core';
import { RegistrationDetails } from '../../shared/entity-interface';
import { hideModal, isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { EntityOverviewService } from '../entity-overview.service';
import { EntityDataStoreService } from '../../entity-data-store.service';

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

    constructor(private _entityOverviewService: EntityOverviewService, private _dataStoreService: EntityDataStoreService) {}

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

}
