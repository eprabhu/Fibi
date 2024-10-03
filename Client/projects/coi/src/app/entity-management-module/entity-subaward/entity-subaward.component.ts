import { Component, OnInit } from '@angular/core';
import { EntityDataStoreService } from '../entity-data-store.service';
import { CommonService } from '../../common/services/common.service';
import { SubawardOrganisationTab } from '../shared/entity-constants';
import { deepCloneObject, isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { EntitySubAwardService, isOrganizationConditionSatisfied } from './entity-subaward.service';
import { EntireEntityDetails, EntityAttachment, EntityDetails, EntityRisk, EntityTabStatus, EntitySectionDetails, SubAwardOrganization, SubAwardOrganizationDetails, SubAwardOrgUpdateClass } from '../shared/entity-interface';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { COMMON_ERROR_TOAST_MSG, ENTITY_VERIFICATION_STATUS, FEED_STATUS_CODE, HTTP_ERROR_STATUS } from '../../app-constants';

@Component({
    selector: 'app-entity-subaward',
    templateUrl: './entity-subaward.component.html',
    styleUrls: ['./entity-subaward.component.scss'],
    providers: [EntitySubAwardService]
})
export class EntitySubawardComponent implements OnInit {

    SUB_AWARD_TAB = SubawardOrganisationTab;
    isLoading = true;
    orgDetailsSubSectionId = '';
    orgDetailsSubSectionName = '';
    entityDetails = new EntityDetails();
    $subscriptions: Subscription[] = [];
    entitySubAwarRisksList: EntityRisk[] = [];
    riskSectionDetails = new EntitySectionDetails();
    entitySubAwarAttachmentList: EntityAttachment[] = [];
    attachmentSectionDetails = new EntitySectionDetails();
    entityTabStatus: EntityTabStatus = new EntityTabStatus();

    constructor(public commonService: CommonService, private _dataStoreService: EntityDataStoreService, private _entitySubAwardService: EntitySubAwardService) { }

    ngOnInit() {
        window.scrollTo(0, 0);
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.setSectionIdAndName();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    private setSectionIdAndName(): void {
        this.riskSectionDetails.subSectionId = 2613;
        this.attachmentSectionDetails.subSectionId = 2614;
        this.orgDetailsSubSectionId = this.commonService.getSectionId(this.SUB_AWARD_TAB, 'SUB_AWARD_ORGANISATION');
        this.orgDetailsSubSectionName = this.commonService.getSectionName(this.SUB_AWARD_TAB, 'SUB_AWARD_ORGANISATION');
        this.attachmentSectionDetails.sectionId = this.commonService.getSectionId(this.SUB_AWARD_TAB, 'SUB_AWARD_ATTACHMENTS');
        this.attachmentSectionDetails.sectionName = this.commonService.getSectionName(this.SUB_AWARD_TAB, 'SUB_AWARD_ATTACHMENTS');
        this.riskSectionDetails.sectionId = this.commonService.getSectionId(this.SUB_AWARD_TAB, 'SUB_AWARD_RISK');
        this.riskSectionDetails.sectionName = this.commonService.getSectionName(this.SUB_AWARD_TAB, 'SUB_AWARD_RISK');
    }

    private getDataFromStore(): void {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        if (this.entityDetails?.entityId != ENTITY_DATA?.entityDetails?.entityId) {
            this.fetchEntityDetails(ENTITY_DATA?.entityDetails?.entityId);
        }
        this.entityDetails = ENTITY_DATA.entityDetails;
        this.entityTabStatus = ENTITY_DATA?.entityTabStatus;
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            }));
    }

    private fetchEntityDetails(entityId: string | number): void {
        this.$subscriptions.push(this._entitySubAwardService.fetchEntityOrganizationDetails(entityId).subscribe((data: SubAwardOrganization) => {
            this.entitySubAwarRisksList = data?.entityRisks || [];
            this.entitySubAwarAttachmentList = data?.attachments || [];
            this._entitySubAwardService.entitySubAwardOrganization.entityRisks = data?.entityRisks || [];
            this._entitySubAwardService.entitySubAwardOrganization.attachments = data.attachments || [];
            this._entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO = data?.subAwdOrgDetailsResponseDTO ?? new SubAwardOrganizationDetails();
            this.isLoading = false;
        }, (_error: any) => {
            this.isLoading = false;
            this.commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
        }));
    }

    riskUpdated(entitySubAwarRisksList: EntityRisk[]): void {
        this.entitySubAwarRisksList = deepCloneObject(entitySubAwarRisksList);
        this._entitySubAwardService.entitySubAwardOrganization.entityRisks = this.entitySubAwarRisksList;
        this.updateCompleteFlag();
        this.updateFeedStatus();
    }

    updateFeedStatus() {
        if (this.entityDetails.entityStatusTypeCode === ENTITY_VERIFICATION_STATUS.VERIFIED && isOrganizationConditionSatisfied(this._entitySubAwardService.entitySubAwardOrganization)) {
            const SUBAWARD_REQ_OBJ: SubAwardOrgUpdateClass = { entityId: this.entityDetails?.entityId, subAwardOrgFields: { feedStatusCode: FEED_STATUS_CODE.READY_TO_FEED } };
            this.$subscriptions.push(this._entitySubAwardService.updateOrganizationDetails(SUBAWARD_REQ_OBJ).subscribe((data: string) => {
                this._dataStoreService.updateFeedStatus(this.entityTabStatus, 'ORG');
            }, err => { this.commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG); }
            ));
        }
    }

    updateCompleteFlag() {
        if (isOrganizationConditionSatisfied(this._entitySubAwardService.entitySubAwardOrganization)) {
            this.entityTabStatus.entity_sub_org_info = true;
            this._dataStoreService.updateStore(['entityTabStatus'], { 'entityTabStatus': this.entityTabStatus });
        }
    }
}
