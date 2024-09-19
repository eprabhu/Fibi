import { Component, OnInit } from '@angular/core';
import { EntityDataStoreService } from '../entity-data-store.service';
import { CommonService } from '../../common/services/common.service';
import { SubawardOrganisationTab } from '../shared/entity-constants';
import { deepCloneObject, isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { EntitySubAwardService, isOrganizationConditionSatisfied } from './entity-subaward.service';
import { EntireEntityDetails, EntityAttachment, EntityDetails, EntityRisk, EntityTabStatus, EntitySectionDetails, SubAwardOrganization, SubAwardOrganizationDetails } from '../shared/entity-interface';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';

@Component({
    selector: 'app-entity-subaward',
    templateUrl: './entity-subaward.component.html',
    styleUrls: ['./entity-subaward.component.scss'],
    providers: [EntitySubAwardService]
})
export class EntitySubawardComponent implements OnInit {

    overViewTab: any;
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
        this.overViewTab = SubawardOrganisationTab;
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
        this.riskSectionDetails.sectionId = this.commonService.getSectionId(this.overViewTab,'SUB_AWARD_RISK');
        this.riskSectionDetails.sectionName = this.commonService.getSectionName(this.overViewTab,'SUB_AWARD_RISK');
        this.orgDetailsSubSectionId = this.commonService.getSectionId(this.overViewTab,'SUB_AWARD_ORGANISATION');
        this.orgDetailsSubSectionName = this.commonService.getSectionName(this.overViewTab,'SUB_AWARD_ORGANISATION');
        this.attachmentSectionDetails.sectionId = this.commonService.getSectionId(this.overViewTab,'SUB_AWARD_ATTACHMENTS');
        this.attachmentSectionDetails.sectionName = this.commonService.getSectionName(this.overViewTab,'SUB_AWARD_ATTACHMENTS');
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
            this.entitySubAwarRisksList = data?.entityRisks ? data?.entityRisks : [];
            this.entitySubAwarAttachmentList = data?.attachments ? data.attachments : [];
            this._entitySubAwardService.entitySubAwardOrganization.entityRisks = data.entityRisks;
            this._entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO = data?.subAwdOrgDetailsResponseDTO ?? new SubAwardOrganizationDetails();
            this.isLoading = false;
        }, (_error: any) => {
            this.isLoading = false;
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }

    riskUpdated(entitySubAwarRisksList: EntityRisk[]): void {
        this.entitySubAwarRisksList = deepCloneObject(entitySubAwarRisksList);
        this._entitySubAwardService.entitySubAwardOrganization.entityRisks = this.entitySubAwarRisksList;
        this.updateHeaderStatus();
    }
    updateHeaderStatus() {
        if(isOrganizationConditionSatisfied(this._entitySubAwardService.entitySubAwardOrganization)) {
             this.entityTabStatus.entity_sub_org_info = true;
             this._dataStoreService.updateStore(['entityTabStatus'], { 'entityTabStatus':  this.entityTabStatus });
        }
     }
}
