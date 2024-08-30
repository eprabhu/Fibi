import { Component, OnInit } from '@angular/core';
import { EntityDataStoreService } from '../entity-data-store.service';
import { CommonService } from '../../common/services/common.service';
import { SubawardOrganisationTab } from '../shared/entity-constants';
import { deepCloneObject, isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { EntitySubAwardService } from './entity-subaward.service';
import { EntireEntityDetails, EntityAttachment, EntityDetails, EntityRisk, SubAwardOrganization, SubAwardOrganizationDetails } from '../shared/entity-interface';
import { HTTP_ERROR_STATUS } from '../../app-constants';

@Component({
    selector: 'app-entity-subaward',
    templateUrl: './entity-subaward.component.html',
    styleUrls: ['./entity-subaward.component.scss'],
    providers: [EntitySubAwardService]
})
export class EntitySubawardComponent implements OnInit {

    overViewTab: any;
    isLoading = true;
    entityDetails = new EntityDetails();
    $subscriptions: Subscription[] = [];
    entitySubAwarRisksList: EntityRisk[] = [];
    entitySubAwarAttachmentList: EntityAttachment[] = [];

    riskSubSectionId = '';
    riskSubSectionName = '';
    orgDetailsSubSectionId = '';
    orgDetailsSubSectionName = '';
    attachmentSubSectionId = '';
    attachmentSubSectionName = '';

    constructor(public commonService: CommonService, private _dataStoreService: EntityDataStoreService, private _entitySubAwardService: EntitySubAwardService) { }

    ngOnInit() {
        window.scrollTo(0, 0);
        this.overViewTab = SubawardOrganisationTab;
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.setSectionIdAndName();
    }

    private setSectionIdAndName(): void {
        this.riskSubSectionId = this.commonService.getSectionId(this.overViewTab,'SUB_AWARD_RISK');
        this.riskSubSectionName = this.commonService.getSectionName(this.overViewTab,'SUB_AWARD_RISK');
        this.orgDetailsSubSectionId = this.commonService.getSectionId(this.overViewTab,'SUB_AWARD_ORGANISATION');
        this.orgDetailsSubSectionName = this.commonService.getSectionName(this.overViewTab,'SUB_AWARD_ORGANISATION');
        this.attachmentSubSectionId = this.commonService.getSectionId(this.overViewTab,'SUB_AWARD_ATTACHMENTS');
        this.attachmentSubSectionName = this.commonService.getSectionName(this.overViewTab,'SUB_AWARD_ATTACHMENTS');
    }

    private getDataFromStore(): void {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        if (this.entityDetails?.entityId != ENTITY_DATA?.entityDetails?.entityId) {
            this.fetchEntityDetails(ENTITY_DATA?.entityDetails?.entityId);
        }
        this.entityDetails = ENTITY_DATA.entityDetails;
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
    }
}
