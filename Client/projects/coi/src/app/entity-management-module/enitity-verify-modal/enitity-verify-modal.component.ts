import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { COIModalConfig, ModalActionEvent } from '../../shared-components/coi-modal/coi-modal.interface';
import { closeCommonModal, openCommonModal } from '../../common/utilities/custom-utilities';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { forkJoin, Subscription } from 'rxjs';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { CommonService } from '../../common/services/common.service';
import { EntityDataStoreService } from '../entity-data-store.service';
import { DuplicateCheckObj, EntireEntityDetails, EntityCardDetails, EntityDetails, EntityDupCheckConfig, EntitySponsor, EntityTabStatus, removeToast, SubAwardOrganization, VerifyModalAction } from '../shared/entity-interface';
import { EntityManagementService } from '../entity-management.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { Router } from '@angular/router';
import { DUPLICATE_MARK_CONFIRMATION_TEXT } from '../shared/entity-constants';

@Component({
    selector: 'app-enitity-verify-modal',
    templateUrl: './enitity-verify-modal.component.html',
    styleUrls: ['./enitity-verify-modal.component.scss']
})
export class EnitityVerifyModalComponent implements OnInit {

    entityDetails = new EntityDetails();
    $subscriptions: Subscription[] = [];
    ENTITY_VERIFY_MODAL_ID: string = 'coi-entity-verify-modal';
    modalSection = {
        entity: false,
        sponsor: false,
        organization: false,
        duplicateMatch: false,
    }
    entitySponsorDetails =  new EntitySponsor();
    matchedDuplicateEntites: EntityCardDetails[] = [];
    entitySubAwardOrganization = new SubAwardOrganization();
    entityTabStatus: EntityTabStatus = new EntityTabStatus();
    attachmentHelpText = 'You are about to verify the entity.';
    DUPLICATE_MARK_CONFIRMATION_TEXT = DUPLICATE_MARK_CONFIRMATION_TEXT;
    entityVerifyModalConfig = new COIModalConfig(this.ENTITY_VERIFY_MODAL_ID, 'Verify', 'Cancel', 'lg');

    @Input() hasConfirmedNoDuplicate = false;
    @Output() verifyModalAction = new EventEmitter<VerifyModalAction>();

    constructor(private _router: Router,
                private _commonService: CommonService,
                private _dataStoreService: EntityDataStoreService,
                private _entityManagementService: EntityManagementService) {}

    ngOnInit(): void {
        this.listenDataChangeFromStore();
        this.getDataFromStore();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private loadEntityDetails(): void {
        forkJoin({
            sponsor: this._entityManagementService.fetchEntitySponsorDetails(this.entityDetails.entityId),
            organization: this._entityManagementService.fetchEntityOrganizationDetails(this.entityDetails.entityId),
            duplicateMatch: this._entityManagementService.checkForDuplicate(this._dataStoreService.getDuplicateCheckRO())
        }).subscribe({
            next: (result) => {
                // set sponsor details
                this.setModalData(result);
                // open modal
                this.openEntityVerifyModal();
            },
            error: (_error: any) => {
                this.closeEntityVerifyModal({ action: 'API_FAILED' });
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
            }
        });
    }

    private setModalData(result: { sponsor: Object; organization: Object; duplicateMatch: any;}) {
        // set sponsor details
        this.entitySponsorDetails = result.sponsor;
        this.modalSection.sponsor = !!this.entitySponsorDetails?.sponsorDetailsResponseDTO?.sponsorType?.code;
        // set organization details
        this.entitySubAwardOrganization = result.organization;
        const { entityRisks, subAwdOrgDetailsResponseDTO } = this.entitySubAwardOrganization || {};
        this.modalSection.organization = !!entityRisks?.length && !!subAwdOrgDetailsResponseDTO?.entityOrganizationType?.organizationTypeCode;
        // set duplicate matching details
        this.matchedDuplicateEntites = result.duplicateMatch ? result.duplicateMatch : [];
        this.matchedDuplicateEntites = this.matchedDuplicateEntites?.filter((entity: EntityCardDetails) => entity?.entityId != this.entityDetails?.entityId);
        this.updateVerifyButtonState();
    }

    private getDataFromStore(): void {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        this.entityDetails = ENTITY_DATA.entityDetails;
        this.modalSection.entity = this._dataStoreService.getIsEntityMandatoryFilled();
        this.updateVerifyButtonState();
        this.loadEntityDetails();
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            }));
    }

    private verify(modalAction: ModalActionEvent): void {
        if (this.modalSection.entity) {
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

    closeEntityVerifyModal(modalAction: VerifyModalAction): void {
        closeCommonModal(this.ENTITY_VERIFY_MODAL_ID);
        setTimeout(() => {
            this.verifyModalAction.emit(modalAction);
        }, 200);
    }

    navigateToSection(navigateTo: 'entity-overview' | 'entity-sponsor' | 'entity-subaward' | 'entity-duplicate'): void {
        const VIEW_MODE_MAP: any = {
            'entity-overview': { action: 'VIEW_OVERVIEW' },
            'entity-sponsor': { action: 'VIEW_SPONSOR' },
            'entity-subaward': { action: 'VIEW_SUBAWARD' },
            'entity-duplicate': { action: 'VIEW_DUPLICATE', event: { hasConfirmedNoDuplicate : this.hasConfirmedNoDuplicate}}
        };
        this.closeEntityVerifyModal(VIEW_MODE_MAP[navigateTo]);
        if (navigateTo !== 'entity-duplicate') {
            this._router.navigate([`/coi/manage-entity/${navigateTo}`], { queryParamsHandling: 'merge' });
        }
    }

    updateVerifyButtonState(): void {
        this.modalSection.duplicateMatch = !(this.matchedDuplicateEntites?.length && !this.hasConfirmedNoDuplicate)
        this.entityVerifyModalConfig.ADAOptions.isDisablePrimaryBtn = !this.modalSection.entity || !this.modalSection.duplicateMatch;
    }

}
