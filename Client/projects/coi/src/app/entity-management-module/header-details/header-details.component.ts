import {Component, ElementRef, OnDestroy, OnInit, ViewChild} from '@angular/core';
import { closeCommonModal, openCoiSlider, openCommonModal, openInNewTab } from '../../common/utilities/custom-utilities';
import { ActivatedRoute, Router } from '@angular/router';
import { deepCloneObject, isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { forkJoin, Subscription } from 'rxjs';
import { EntityDataStoreService } from '../entity-data-store.service';
import {
    DNBReqObj,
    DuplicateActionType,
    DuplicateCheckObj,
    EntireEntityDetails,
    EntityCardDetails,
    EntityDetails,
    EntityDupCheckConfig,
    EntityTabStatus,
    removeToast,
    VerifyModalAction
} from '../shared/entity-interface';
import { AutoSaveService } from '../../common/services/auto-save.service';
import {subscriptionHandler} from "../../../../../fibi/src/app/common/utilities/subscription-handler";
import { EntityManagementService, getEntityFullAddress } from '../entity-management.service';
import { CommonService } from '../../common/services/common.service';
import { COIModalConfig } from '../../shared-components/coi-modal/coi-modal.interface';
import { COMMON_ERROR_TOAST_MSG, ENTITY_DOCUMNET_STATUS_TYPE, ENTITY_VERIFICATION_STATUS, FEED_STATUS_CODE, HTTP_ERROR_STATUS } from '../../app-constants';
import { DUPLICATE_MARK_CONFIRMATION_TEXT, DUPLICATE_MARK_INFORMATION_TEXT } from '../shared/entity-constants';

@Component({
  selector: 'app-header-details',
  templateUrl: './header-details.component.html',
  styleUrls: ['./header-details.component.scss'],
})
export class HeaderDetailsComponent implements OnInit, OnDestroy {

    @ViewChild('mainEntityHeaders', { static: true }) mainEntityHeaders: ElementRef;

    sliderElementId: any;
    isShowOptions = false;
    showSlider = false;
    isShowNavBarOverlay = false;
    $subscriptions: Subscription[] = [];
    entityDetails: EntityDetails = new EntityDetails();
    entityFullAddress: string = '';
    latestPriorName: any;
    isEditMode = false;
    matchedEntites: any;
    isOpenVerifyModal = false;
    isOpenEntityDuplicate = false;
    entityTabStatus = new EntityTabStatus();
    canVerifyEntity = false;
    canManageEntity = false;
    ENTITY_DUNS_MATCH_CONFIRMATION_MODAL_ID: string = 'use_duns_match_entity_confirmation_modal';
    dunsMatchConfirmationModalConfig = new COIModalConfig(this.ENTITY_DUNS_MATCH_CONFIRMATION_MODAL_ID, 'Use this', 'Cancel', '');
    selectedDUNSNumber: string;
    isSaving = false;
    cardDetails: EntityCardDetails[] = [];
    canModifyEntity = false;
    duplicateEntityDetails = new EntityCardDetails();
    badgeClass: string;
    originalEntityName: string;
    dupCheckPayload: DuplicateCheckObj;
    entityDupCheckConfig = new EntityDupCheckConfig();
    ENTITY_VERIFIED = ENTITY_VERIFICATION_STATUS.VERIFIED;
    ENTITY_UNVERIFIED = ENTITY_VERIFICATION_STATUS.UNVERIFIED;
    ENTITY_DUPLICATE = ENTITY_DOCUMNET_STATUS_TYPE.DUPLICATE;

    constructor(public router: Router, public dataStore: EntityDataStoreService,
        public autoSaveService: AutoSaveService,
        private _entityManagementService: EntityManagementService,
        public commonService: CommonService,
    ) { }
    ngOnInit() {
        this.dunsMatchConfirmationModalConfig.dataBsOptions.focus = false;
        this.dunsMatchConfirmationModalConfig.dataBsOptions.keyboard = true;
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    viewSlider(event) {
        this.cardDetails = [];
        this.$subscriptions.push(this._entityManagementService.getDunsMatch(this.getReqObj()).subscribe((data: any) => {
        this.matchedEntites = data?.matchCandidates?.length ? data?.matchCandidates : [];
        if(this.matchedEntites.length) {
            this.matchedEntites.forEach((ele: any) => {
                this.cardDetails.push(this.formatResponse(ele));
            })
        }
        this.showSlider = event;
        this.sliderElementId = 'duns-match-slider';
        setTimeout(() => {
            openCoiSlider(this.sliderElementId);
        });
    }))
    }

    getReqObj(): any {
        let reqObj = new DNBReqObj();
        reqObj.sourceDataName = this.entityDetails.entityName;
        reqObj.sourceDunsNumber = this.entityDetails.dunsNumber || '';
        reqObj.addressLine1 = this.entityDetails.primaryAddressLine1;
        reqObj.addressLine2 = this.entityDetails.primaryAddressLine2 || '';
        reqObj.countryCode = this.entityDetails?.country?.countryTwoCode;
        reqObj.state = '';
        reqObj.postalCode = this.entityDetails.postCode || '';
        reqObj.emailAddress = this.entityDetails.certifiedEmail || '';
        return reqObj;
    }

    validateSliderClose() {
        closeCommonModal(this.ENTITY_DUNS_MATCH_CONFIRMATION_MODAL_ID);
        setTimeout(() => {
            this.showSlider = false;
            this.isOpenEntityDuplicate = false;
            this.sliderElementId = '';
		}, 500);
	  }

    onClickMenuBar() {
        this.updateFeedStatus();
        const NAV_ELEMENT = document.getElementById('responsive-nav');
        const IS_MENU_SHOW = NAV_ELEMENT.classList.contains('show-menu');
        const IS_SCREEN = window.innerWidth <= 1300;

        if (IS_MENU_SHOW) {
            NAV_ELEMENT.classList.remove('show-menu');
            if (IS_SCREEN) {
                this.isShowNavBarOverlay = false;
            }
        } else {
            if (IS_SCREEN) {
                this.isShowNavBarOverlay = true;
            }
            NAV_ELEMENT.classList.toggle('show-menu', IS_SCREEN);
        }
    }

    private getDataFromStore() {
        const ENTITY_DATA: EntireEntityDetails = this.dataStore.getData();
        if (!ENTITY_DATA || isEmptyObject(ENTITY_DATA)) { return; }
        this.entityDetails = ENTITY_DATA.entityDetails;
        this.latestPriorName = ENTITY_DATA?.priorNames?.[0]?.priorNames;
        this.entityTabStatus = ENTITY_DATA.entityTabStatus;
        this.entityTabStatus.entity_overview = this.dataStore.getIsEntityMandatoryFilled();
        this.entityFullAddress = getEntityFullAddress(this.entityDetails);
        this.isEditMode = this.dataStore.getEditMode();
        this.canModifyEntity = this.getCanModifyEntity();
        this.badgeClass = this.getBadgeClass();
        this.dupCheckPayload = this.dataStore.getDuplicateCheckRO();
        this.originalEntityName = ENTITY_DATA?.originalName;
        this.checkUserHasRight();
    }

    getBadgeClass(): string {
       return this.entityDetails?.entityDocumentStatusType?.documentStatusTypeCode === ENTITY_DOCUMNET_STATUS_TYPE.DUPLICATE ? 'text-bg-warning' : 'text-bg-success';
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this.dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    formatResponse(entity) {
        let entityDetails: EntityCardDetails = new EntityCardDetails();
        entityDetails.entityName = entity.organization.primaryName || '';
        entityDetails.state = entity.organization?.primaryAddress?.addressRegion?.abbreviatedName || '';
        entityDetails.dunsNumber = entity.organization.duns;
        entityDetails.primaryAddress = (entity.organization?.primaryAddress?.streetAddress?.line1 ? entity.organization?.primaryAddress?.streetAddress?.line1 : '') +
        (entity.organization?.primaryAddress?.streetAddress?.line2 ? ','+ entity.organization?.primaryAddress?.streetAddress?.line2 : '' );
        entityDetails.city = entity.organization?.primaryAddress?.addressLocality?.name || '';
        entityDetails.country = entity.organization?.primaryAddress?.addressCountry?.name || '';
        entityDetails.phone = entity?.organization?.telephone[0]?.telephoneNumber || '';
        entityDetails.postalCode = entity.organization?.primaryAddress?.postalCode || '';
        entityDetails.matchQualityInformation = entity?.matchQualityInformation?.confidenceCode;
        entityDetails.duplicateEntityDetails = entity?.entity ? deepCloneObject(entity?.entity) : null;
        return entityDetails;
    }

    openVerifyEntityModal(hasConfirmedNoDuplicate = false): void {
        this.isOpenVerifyModal = true;
        this.entityDupCheckConfig.hasConfirmedNoDuplicate = hasConfirmedNoDuplicate;
    }

    verifyModalAction(modalAction: VerifyModalAction): void {
        this.isOpenVerifyModal = false;
        if (modalAction.action === 'VIEW_DUPLICATE') {
            this.entityDupCheckConfig.duplicateView = 'SLIDER_VIEW';
            this.entityDupCheckConfig.primaryButton = '';
            this.entityDupCheckConfig.confirmationText = DUPLICATE_MARK_CONFIRMATION_TEXT;
            this.entityDupCheckConfig.infoText = DUPLICATE_MARK_INFORMATION_TEXT;
            this.entityDupCheckConfig.header = 'Potential Entity Duplicates'
            this.entityDupCheckConfig.hasConfirmedNoDuplicate = modalAction.event?.hasConfirmedNoDuplicate;
            this.entityDupCheckConfig.triggeredFrom = 'ENTITY_VERIFY';
            this.entityDupCheckConfig.entityIdToFilter = this.entityDetails.entityId;
            this.isOpenEntityDuplicate = true;
        }
    }

    navigateToBack() {
        this.router.navigate(['/coi/entity-dashboard'])
    }

    checkUserHasRight(): void {
        this.canVerifyEntity = this.commonService.getAvailableRight(['VERIFY_ENTITY'], 'SOME');
        this.canManageEntity = this.commonService.getAvailableRight(['MANAGE_ENTITY'], 'SOME');
    }

    getCanModifyEntity(): boolean {
        return this.commonService.getAvailableRight(['MANAGE_ENTITY', 'MANAGE_ENTITY_ORGANIZATION', 'MANAGE_ENTITY_COMPLIANCE', 'MANAGE_ENTITY_SPONSOR'], 'SOME') &&
        !this.commonService.isEntityModified && this.entityDetails?.entityStatusType?.entityStatusTypeCode == ENTITY_VERIFICATION_STATUS.VERIFIED && !this.isEditMode &&
        this.entityDetails?.entityDocumentStatusType?.documentStatusTypeCode === ENTITY_DOCUMNET_STATUS_TYPE.ACTIVE;
    }
    openConfirmationModal(event: 'USE' | 'OPEN_MODAL',entity: EntityCardDetails) {
        if(event === 'USE') {
            this.selectedDUNSNumber = entity.dunsNumber;
            openCommonModal(this.ENTITY_DUNS_MATCH_CONFIRMATION_MODAL_ID);
        } else if(event === 'OPEN_MODAL') {
            this.duplicateEntityDetails = deepCloneObject(entity.duplicateEntityDetails);
        }
    }

    openDuplicateConfirmationModal(event: {action: 'OPEN_MODAL', event: any}) {
        if(event.action === 'OPEN_MODAL') {
            this.duplicateEntityDetails = deepCloneObject(event.event);
        }
    }

    callEnrichAPI(event) {
        if (event.action === 'PRIMARY_BTN') {
            this.validateSliderClose();
            this.triggerEnrichAPICall();
        }
        closeCommonModal(this.ENTITY_DUNS_MATCH_CONFIRMATION_MODAL_ID);
    }

    triggerEnrichAPICall() {
        if (!this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(this._entityManagementService.triggerEnrichAPI({
                duns: this.selectedDUNSNumber,
                entityId: this.entityDetails.entityId,
                actionPersonId: this.commonService.getCurrentUserDetail('personID')
            }).subscribe((data: any) => {
                if (data?.httpStatusCode == '200') {
                    this.updateEntityDetails();
                } else {
                    this.isSaving = false;
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, please try again.');
                }
            }, error => {
                this.isSaving = false;
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, please try again.');
            }
            ));
        }
    }

    updateEntityDetails() {
        this.$subscriptions.push(forkJoin(this.generatHTTPRequest()).subscribe((response: any) => {
            if (response.length) {
                this.isSaving = false;
                if (response[0]) {
                    this.dataStore.setStoreData(response[0]);
                }
                if (response[1]) {
                    this.entityDetails.isDunsMatched = true;
                    this.dataStore.updateStore(['entityDetails'], { 'entityDetails': this.entityDetails })
                }
            }

        }, error => {
            this.isSaving = false;
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, please try again.');
        }));
    }

    generatHTTPRequest() {
        const httpRequest = [];
        httpRequest.push(this._entityManagementService.getEntityDetails(this.entityDetails.entityId));
        httpRequest.push(this._entityManagementService.updateIsDUNSMatchFlag({
            entityId: this.entityDetails.entityId,
            isDunsMatched: true
        }));
        return httpRequest;
    }

    leaveSlider() {
        removeToast('ERROR');
        removeToast('SUCCESS');
        this.commonService.setChangesAvailable(false);
    }

    resetNavigationStop() {
        this.commonService.isNavigationStopped = false;
        this.commonService.attemptedPath = '';
    }

    modifyEntity(): void {
        const REQ_OBJ = { entityId: this.entityDetails.entityId, actionLogCode: 15 };
        this.$subscriptions.push(this._entityManagementService.logFeedHistory(REQ_OBJ).subscribe((data: any) => {
            if (data) {
                this.dataStore.updateModifiedFlag(this.entityDetails, true);
                this.updateFeedStatus();
            }
        }, err => {
            this.commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
        }));
    }

    openEntity(): void{
        openInNewTab('manage-entity/entity-overview?', ['entityManageId'], [this.entityDetails?.originalEntityId]);
    }

    duplicateCheckResponse(event: {action: DuplicateActionType, event?: any}): void {
        this.isOpenEntityDuplicate = false;
        this.entityDupCheckConfig.duplicateView = '';
        if (this.entityDupCheckConfig.triggeredFrom === 'ENTITY_VERIFY' || event.action === 'API_FAILED') {
            this.openVerifyEntityModal(event.event?.hasConfirmedNoDuplicate);
        }
    }

    getCurrentTime() {
        return new Date().toLocaleTimeString('en-US', {hour: '2-digit', minute: '2-digit'});
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    updateFeedStatus() {
        if((this.entityTabStatus.sponsor_feed_status_code === FEED_STATUS_CODE.READY_TO_FEED || this.entityTabStatus.organization_feed_status_code === FEED_STATUS_CODE.READY_TO_FEED ) &&
          this.entityDetails?.entityStatusType?.entityStatusTypeCode == ENTITY_VERIFICATION_STATUS.VERIFIED) {
            this.$subscriptions.push(this._entityManagementService.getEntityDetails(this.entityDetails.entityId).subscribe((response: any) => {
                if (response) {
                    this.dataStore.setStoreData(response);
                }
            }));
        }
    }


}
