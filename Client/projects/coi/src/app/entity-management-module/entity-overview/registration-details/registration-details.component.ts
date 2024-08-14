import { Component } from '@angular/core';
import { RegistrationDetails } from '../../shared/entity-interface';
import { hideModal, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { EntityOverviewService } from '../entity-overview.service';

@Component({
  selector: 'app-registration-details',
  templateUrl: './registration-details.component.html',
  styleUrls: ['./registration-details.component.scss']
})
export class RegistrationDetailsComponent {

    registrationDetails: RegistrationDetails = new RegistrationDetails();
    entityRegistrationTypeOption = 'ENTITY_REGISTRATION_TYPE#ENTITY_REGISTRATION_TYPE#true#true';
    mandatoryList = new Map();
    $subscriptions: Subscription[] = [];

    constructor(private _entityOverviewService: EntityOverviewService) {}

    addRegistrationDetails(event) {
        if(event) {
            openModal('addRegistrationDetails');
        }
    }

    clearRegistrationDetails() {
        this.registrationDetails = new RegistrationDetails();
        this.mandatoryList.clear();
        hideModal('addRegistrationDetails');
    }

    addRegistration() {
        this.entityMandatoryValidation();
        if(!this.mandatoryList.size) {
            this.$subscriptions.push(this._entityOverviewService.addRegistrationDetails(this.registrationDetails).subscribe((data: any) => {
                console.log(data);
                this.clearRegistrationDetails();
            }))
        }
    }

    entityRegistrationTypeSelect(event) {
        if(event) {
            this.registrationDetails.regTypeCode = event;
        } else {
            this.registrationDetails.regTypeCode = null;
        }
    }
    entityMandatoryValidation(): void {
        this.mandatoryList.clear();
        // if(!this.registrationDetails.regTypeCode) {
        //     this.mandatoryList.set('registrationType', 'Please select registration type.');
        // }
        if(!this.registrationDetails.regNumber) {
            this.mandatoryList.set('registrationNumber', 'Please enter registration number.');
        }
    }

}
