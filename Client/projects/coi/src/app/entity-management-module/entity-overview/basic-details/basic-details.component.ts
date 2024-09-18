import {Component, HostListener, Input, OnDestroy, OnInit} from '@angular/core';
import { Country, Create_Entity, EntityDetails, EntityTabStatus } from '../../shared/entity-interface';
import { isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntityDataStoreService } from '../../entity-data-store.service';
import {subscriptionHandler} from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subject, Subscription } from 'rxjs';
import { EntityManagementService } from '../../entity-management.service';
import {CommonService} from '../../../common/services/common.service';

@Component({
  selector: 'app-basic-details',
  templateUrl: './basic-details.component.html',
  styleUrls: ['./basic-details.component.scss']
})
export class BasicDetailsComponent implements OnInit, OnDestroy {
    createEntityObj: Create_Entity = new Create_Entity();
    saveObj: Create_Entity = new Create_Entity();
    isEditMode = false;
    @Input() sectionName: any;
    @Input() sectionId: any;
    $subscriptions: Subscription[] = [];
    entityDetails: EntityDetails = new EntityDetails();
    entityCountryDetails: Country = new Country();
    entityTabStatus: EntityTabStatus = new EntityTabStatus();

    constructor(public dataStore: EntityDataStoreService, private _entityManagementService: EntityManagementService, private _commonService: CommonService) {}

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.checkUserHasRight();
    }

    private getDataFromStore() {
        const entityData = this.dataStore.getData();
        if (isEmptyObject(entityData)) { return; }
        this.entityDetails = entityData.entityDetails;
        this.setCreateObj(this.entityDetails);
        this.entityCountryDetails = entityData?.entityDetails?.country;
        this.isEditMode = this.dataStore.getEditMode();
        this.entityTabStatus = entityData?.entityTabStatus;
    }

    setCreateObj(data) {
        if(data) {
            this.createEntityObj.entityName = data['entityName'];
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
            this.createEntityObj.humanSubAssurance = data['humanSubAssurance'];
            this.createEntityObj.anumalWelfareAssurance = data['anumalWelfareAssurance'];
            this.createEntityObj.animalAccreditation = data['animalAccreditation'];
            this.createEntityObj.phoneNumber = data['phoneNumber'];
            this.createEntityObj.entityOwnerShip = data['entityOwnershipType'];
            this.createEntityObj.entityOwnershipTypeCode = data['entityOwnershipTypeCode'];
            this.createEntityObj.postCode = data['postCode'];
        }
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this.dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    updateStoreData(event) {
        if (!isEmptyObject(event?.autoSaveRO)) {
            Object.keys(event?.autoSaveRO).forEach((ele) => {
                this.entityDetails[ele] = event?.autoSaveRO[ele];
            });
            this.dataStore.updateStore(['entityDetails'], { 'entityDetails':  this.entityDetails });
        }
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    checkUserHasRight(): void {
        const hasRight = this._commonService.getAvailableRight(['MANAGE_ENTITY'], 'SOME');
        if (!hasRight) {
            this.isEditMode = false;
        }
    }

}
