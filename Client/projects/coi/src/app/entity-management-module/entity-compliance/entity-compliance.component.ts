import {Component, OnDestroy, OnInit} from '@angular/core';
import { EntityDataStoreService } from '../entity-data-store.service';
import { CommonService } from '../../common/services/common.service';
import { ComplianceTab} from '../shared/entity-constants';
import { EntityComplianceService } from './entity-compliance.service';
import { DataStoreEvent, EntireEntityDetails, EntityAttachment, EntityDetails, EntityRisk, EntitySectionDetails, SubAwardOrganization } from '../shared/entity-interface';
import { Subscription } from 'rxjs';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { isEmptyObject } from '../../common/utilities/custom-utilities';

@Component({
    selector: 'app-entity-compliance',
    templateUrl: './entity-compliance.component.html',
    styleUrls: ['./entity-compliance.component.scss'],
    providers: [EntityComplianceService]
})
export class EntityComplianceComponent implements OnInit, OnDestroy {

    overViewTab: any;
    isLoading = true;
    entityDetails = new EntityDetails();
    $subscriptions: Subscription[] = [];
    complianceEntityRiskList: EntityRisk[] = [];
    entityComplianceAttachmentList: EntityAttachment[] = [];
    riskSectionDetails = new EntitySectionDetails();
    attachmentSectionDetails = new EntitySectionDetails();
    questionnaireSectionDetails = new EntitySectionDetails();


    constructor(public commonService: CommonService, private _dataStoreService: EntityDataStoreService, private _entityComplianceService: EntityComplianceService) { }

    ngOnInit() {
        window.scrollTo(0, 0);
        this.overViewTab = ComplianceTab;
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.setSectionIdAndName();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    private setSectionIdAndName(){
        this.riskSectionDetails.sectionId = this.commonService.getSectionId(this.overViewTab,'COMPLIANCE_RISK');
        this.riskSectionDetails.sectionName = this.commonService.getSectionName(this.overViewTab,'COMPLIANCE_RISK');
        this.riskSectionDetails.subSectionId = 2615;
        this.attachmentSectionDetails.sectionId = this.commonService.getSectionId(this.overViewTab,'COMPLIANCE_ATTACHMENTS');
        this.attachmentSectionDetails.sectionName = this.commonService.getSectionName(this.overViewTab,'COMPLIANCE_ATTACHMENTS');
        this.attachmentSectionDetails.subSectionId = 2616;
        this.questionnaireSectionDetails.sectionId = this.commonService.getSectionId(this.overViewTab,'COMPLIANCE_QUESTIONNAIRE');
        this.questionnaireSectionDetails.sectionName = this.commonService.getSectionName(this.overViewTab,'COMPLIANCE_QUESTIONNAIRE');
        this.questionnaireSectionDetails.subSectionId = this.commonService.getSubSectionId(this.overViewTab,'COMPLIANCE_QUESTIONNAIRE');
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
            this._dataStoreService.dataEvent.subscribe((dependencies: DataStoreEvent) => {
                this.getDataFromStore();
            })
        );
    }

    private fetchEntityDetails(entityId: string | number): void {
        this.$subscriptions.push(this._entityComplianceService.fetchEntityComplianceDetails(entityId).subscribe((data: SubAwardOrganization) => {
            this._entityComplianceService.entityCompliance = data;
            this.complianceEntityRiskList = data?.entityRisks ? data.entityRisks : [];
            this.entityComplianceAttachmentList = data?.attachments ? data.attachments : [];
            this.isLoading = false;
        }, (_error: any) => {
            this.isLoading = false;
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }


}
