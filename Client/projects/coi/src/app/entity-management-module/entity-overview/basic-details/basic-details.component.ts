import { Component, HostListener, Input } from '@angular/core';
import { isValidEmailAddress, inputRestrictionForNumberField, phoneNumberValidation } from '../../../common/utilities/custom-utilities';
import { getEndPointOptionsForCountry } from 'projects/fibi/src/app/common/services/end-point.config';
import { Country, Create_Entity, EntityDetails } from '../../shared/entity-interface';
import { isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { Subscription } from 'rxjs';

@Component({
  selector: 'app-basic-details',
  templateUrl: './basic-details.component.html',
  styleUrls: ['./basic-details.component.scss']
})
export class BasicDetailsComponent {
    createEntityObj: Create_Entity = new Create_Entity();
    saveObj: Create_Entity = new Create_Entity();
    isCreateScreen = false;
    @Input() sectionName: any;
    @Input() sectionId: any;
    $subscriptions: Subscription[] = [];
    entityDetails: EntityDetails = new EntityDetails();
    entityCountryDetails: Country = new Country();

    constructor(public dataStore: EntityDataStoreService) {}

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    saveBasicEntityDetails(event) {
        this.saveObj = event.createEntityObj;
        openModal('entityProceedCheckMatch');
    }

    private getDataFromStore() {
        const entityData = this.dataStore.getData();
        if (isEmptyObject(entityData)) { return; }
        this.entityDetails = entityData.entityDetails;
        this.setCreateObj(this.entityDetails);
        this.entityCountryDetails = entityData?.entityDetails?.country;
    }

    setCreateObj(data) {
        if(data) {
            this.createEntityObj.primaryName = data['primaryName'];
            this.createEntityObj.primaryAddressLine1 = data['primaryAddressLine1'];
            this.createEntityObj.primaryAddressLine2 = data['primaryAddressLine2'];
            this.createEntityObj.city = data['city'];
            this.createEntityObj.countryCode = data['countryCode'];
            this.createEntityObj.state = data['state'];
            this.createEntityObj.websiteAddress = data['websiteAddress'];
            this.createEntityObj.certifiedEmail = data['certifiedEmail'];
            this.createEntityObj.cageNumber = data['cageNumber'];
            this.createEntityObj.ueiNumber= data['ueiNumber'];
            this.createEntityObj.dunsNumber = data['dunsNumber'];
        }
    }

    createEntity() {
        console.log(this.createEntityObj);
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this.dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

}
