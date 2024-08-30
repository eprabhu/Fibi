import { Component, EventEmitter, OnInit, Output } from '@angular/core';
import { COIModalConfig, ModalActionEvent } from '../../shared-components/coi-modal/coi-modal.interface';
import { closeCommonModal, openCommonModal } from '../../common/utilities/custom-utilities';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { CommonService } from '../../common/services/common.service';
import { EntityDataStoreService } from '../entity-data-store.service';
import { EntireEntityDetails, EntityDetails, EntitySponsor, SubAwardOrganization } from '../shared/entity-interface';
import { EntityManagementService } from '../entity-management.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';

@Component({
    selector: 'app-enitity-verify-modal',
    templateUrl: './enitity-verify-modal.component.html',
    styleUrls: ['./enitity-verify-modal.component.scss']
})
export class EnitityVerifyModalComponent implements OnInit {

    entityDetails = new EntityDetails();
    $subscriptions: Subscription[] = [];
    ENTITY_VERIFY_MODAL_ID: string = 'coi-entity-verify-modal';
    isComplete = {
        entity: false,
        sponsor: false,
        organization: false,
        duplicate: false,
    }
    entitySponsorDetails =  new EntitySponsor();
    entitySubAwardOrganization = new SubAwardOrganization();
    entityVerifyModalConfig = new COIModalConfig(this.ENTITY_VERIFY_MODAL_ID, 'Verify', 'Cancel', 'lg');

    @Output() verifyModalAction: EventEmitter<ModalActionEvent> = new EventEmitter<ModalActionEvent>();

    constructor(private _dataStoreService: EntityDataStoreService,
        private _commonService: CommonService, private _entityManagementService: EntityManagementService) {}

    ngOnInit(): void {
        this.listenDataChangeFromStore();
        this.getDataFromStore();
        this.fetchEntitySponsorDetails();
        this.fetchEntityOrganizationDetails();
        this.openEntityVerifyModal();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private getDataFromStore(): void {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        this.entityDetails = ENTITY_DATA.entityDetails;
        // All Mandatory fields(Entity Name,Ownership Type,Addres 1, City,State,Country,Zip/postal code)
        const { entityName, entityOwnershipTypeCode, primaryAddressLine1 , city, state, country, postCode} = this.entityDetails;
        this.isComplete.entity = !!entityName && !!entityOwnershipTypeCode && !!primaryAddressLine1 && !!city && !!state && !!country && !!postCode;
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            }));
    }

    entityVerifyModalActions(modalAction: ModalActionEvent): void {
        switch (modalAction.action) {
            case 'CLOSE_BTN':
            case 'SECONDARY_BTN':
                return this.closeEntityVerifyModal(modalAction);
            case 'PRIMARY_BTN':
                return this.verify(modalAction);
            default: break;
        }
    }

    openEntityVerifyModal(): void {
        setTimeout(() => {
            openCommonModal(this.ENTITY_VERIFY_MODAL_ID);
        }, 200);
    }

    closeEntityVerifyModal(modalAction: ModalActionEvent): void {
        closeCommonModal(this.ENTITY_VERIFY_MODAL_ID);
        setTimeout(() => {
            this.verifyModalAction.emit(modalAction);
        }, 200);
    }

    private verify(modalAction: ModalActionEvent): void {
        if (this.isComplete.entity) {
            this.$subscriptions.push(
                this._entityManagementService.verifyEntity(this.entityDetails.entityId)
                    .subscribe((res) => {
                        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Entity verified successfully.');
                        this.entityDetails.entityStatusType ={
                            entityStatusTypeCode: "1",
                            description: "Verified",
                        }
                        this._dataStoreService.updateStore(['entityDetails'], { entityDetails: this.entityDetails })
                        this.closeEntityVerifyModal(modalAction);
                    }, (_error: any) => {
                        this._commonService.showToast(HTTP_ERROR_STATUS, 'Entity verification failed. Please try again.');
                    }));
        } else {
            this._commonService.showToast(HTTP_ERROR_STATUS, `Please fill all the mandatory fields in 'Entity Information'`);
        }

    }

    fetchEntitySponsorDetails(): void{
        this.$subscriptions.push(
            this._entityManagementService.fetchEntitySponsorDetails(this.entityDetails.entityId)
                .subscribe((data: EntitySponsor)=>{
                this.entitySponsorDetails = data;
                this.isComplete.sponsor = !!this.entitySponsorDetails?.sponsorDetailsResponseDTO?.sponsorTypeCode;
            }, (_error: any) => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
            }));
    }

    fetchEntityOrganizationDetails(): void{
        this.$subscriptions.push(
            this._entityManagementService.fetchEntityOrganizationDetails(this.entityDetails.entityId)
                .subscribe((data: SubAwardOrganization)=>{
                this.entitySubAwardOrganization = data;
                this.isComplete.organization = !!this.entitySubAwardOrganization?.entityRisks?.length && !!this.entitySubAwardOrganization?.subAwdOrgDetailsResponseDTO?.organizationTypeCode;
            }, (_error: any) => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
            }));
    }

}

