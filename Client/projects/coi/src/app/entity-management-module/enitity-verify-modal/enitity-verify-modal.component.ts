import { Component, EventEmitter, OnInit, Output } from '@angular/core';
import { COIModalConfig, ModalActionEvent } from '../../shared-components/coi-modal/coi-modal.interface';
import { closeCommonModal, openCommonModal } from '../../common/utilities/custom-utilities';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { forkJoin, Subscription } from 'rxjs';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { CommonService } from '../../common/services/common.service';
import { EntityDataStoreService } from '../entity-data-store.service';
import { EntireEntityDetails, EntityDetails, EntitySponsor, EntityTabStatus, removeToast, SubAwardOrganization } from '../shared/entity-interface';
import { EntityManagementService } from '../entity-management.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { Router } from '@angular/router';

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
    attachmentHelpText = 'You are about to verify the entity.'
    entityTabStatus: EntityTabStatus = new EntityTabStatus();

    @Output() verifyModalAction: EventEmitter<ModalActionEvent | null> = new EventEmitter<ModalActionEvent | null>();

    constructor(private _router: Router,
                private _commonService: CommonService,
                private _dataStoreService: EntityDataStoreService,
                private _entityManagementService: EntityManagementService) {}

    ngOnInit(): void {
        this.listenDataChangeFromStore();
        this.getDataFromStore();
        this.loadEntityDetails();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private loadEntityDetails(): void {
        forkJoin({
            sponsor: this._entityManagementService.fetchEntitySponsorDetails(this.entityDetails.entityId),
            organization: this._entityManagementService.fetchEntityOrganizationDetails(this.entityDetails.entityId)
        }).subscribe({
            next: (result) => {
                // set sponsor details
                this.setSponsorAndOrgaizationData(result);
                // open modal
                this.openEntityVerifyModal();
            },
            error: (_error: any) => {
                this.closeEntityVerifyModal(null);
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
            }
        });
    }

    private setSponsorAndOrgaizationData(result: { sponsor: Object; organization: Object; }) {
        this.entitySponsorDetails = result.sponsor;
        this.isComplete.sponsor = !!this.entitySponsorDetails?.sponsorDetailsResponseDTO?.sponsorType?.code;
        // set organization details
        this.entitySubAwardOrganization = result.organization;
        const { entityRisks, subAwdOrgDetailsResponseDTO } = this.entitySubAwardOrganization || {};
        this.isComplete.organization = !!entityRisks?.length && !!subAwdOrgDetailsResponseDTO?.entityOrganizationType?.organizationTypeCode;
    }

    private getDataFromStore(): void {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        this.entityDetails = ENTITY_DATA.entityDetails;
        this.isComplete.entity = this._dataStoreService.getIsEntityMandatoryFilled();
        this.entityVerifyModalConfig.ADAOptions.isDisablePrimaryBtn = !this.isComplete.entity;
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            }));
    }

    private verify(modalAction: ModalActionEvent): void {
        if (this.isComplete.entity) {
            this.$subscriptions.push(
                this._entityManagementService.verifyEntity(this.entityDetails.entityId)
                    .subscribe((res: any) => {
                        this.entityTabStatus = res;
                        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Entity verified successfully.');
                        this.entityDetails.entityStatusType = {
                            entityStatusTypeCode: "1",
                            description: "Verified",
                        };
                        this.entityDetails.entityStatusTypeCode = '1';
                        this._dataStoreService.updateStore(['entityDetails', 'entityTabStatus'], { entityDetails: this.entityDetails, entityTabStatus: this.entityTabStatus})
                        this.closeEntityVerifyModal(modalAction);
                        removeToast('ERROR');
                        removeToast('SUCCESS');
                        this.navigateToSection('entity-overview');
                    }, (_error: any) => {
                        this._commonService.showToast(HTTP_ERROR_STATUS, 'Entity verification failed. Please try again.');
                    }));
        } else {
            this._commonService.showToast(HTTP_ERROR_STATUS, `Please fill all the mandatory fields in 'Entity Information'`);
        }
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

    closeEntityVerifyModal(modalAction: ModalActionEvent | null): void {
        closeCommonModal(this.ENTITY_VERIFY_MODAL_ID);
        setTimeout(() => {
            this.verifyModalAction.emit(modalAction);
        }, 200);
    }

    navigateToSection(navigateTo: 'entity-overview' | 'entity-sponsor' | 'entity-subaward'): void {
        this.closeEntityVerifyModal(null);
        this._router.navigate([`/coi/manage-entity/${navigateTo}`], { queryParamsHandling: 'merge' } );
    }

}

