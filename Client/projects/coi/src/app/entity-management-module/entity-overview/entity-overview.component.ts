import { Component } from '@angular/core';
import { OverviewTabSection } from '../shared/entity-constants';
import { CommonService } from '../../common/services/common.service';
import { EntityDataStoreService } from '../entity-data-store.service';
import { deepCloneObject, isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntireEntityDetails, EntityAttachment, EntityRisk, EntitySectionDetails } from '../shared/entity-interface';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';

@Component({
    selector: 'app-entity-overview',
    templateUrl: './entity-overview.component.html',
    styleUrls: ['./entity-overview.component.scss']
})
export class EntityOverviewComponent {

    overviewTab: any;
    coiEntity: any;
    entityRisksList: EntityRisk[] = [];
    $subscriptions: Subscription[] = [];
    entityAttachmentList: EntityAttachment[] = [];
    basicDetails = new EntitySectionDetails();
    companyDetails = new EntitySectionDetails();
    otherReferences = new EntitySectionDetails();
    riskSectionDetails = new EntitySectionDetails();
    attachmentSectionDetails = new EntitySectionDetails();

    constructor(public commonService: CommonService, public _dataStoreService: EntityDataStoreService) {}

    ngOnInit() {
        window.scrollTo(0, 0);
        this.overviewTab = OverviewTabSection;
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.setSectionIdAndName();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    private setSectionIdAndName(){
        this.riskSectionDetails.subSectionId = 2607;
        this.attachmentSectionDetails.subSectionId = 2623;
        this.basicDetails.sectionId = this.commonService.getSectionId(this.overviewTab,'BASIC_DETAILS');
        this.basicDetails.sectionName = this.commonService.getSectionName(this.overviewTab,'BASIC_DETAILS');
        this.companyDetails.sectionId = this.commonService.getSectionId(this.overviewTab,'COMPANY_DETAILS');
        this.companyDetails.sectionName = this.commonService.getSectionName(this.overviewTab,'COMPANY_DETAILS');
        this.riskSectionDetails.sectionId = this.commonService.getSectionId(this.overviewTab,'ENTITY_RISK');
        this.riskSectionDetails.sectionName = this.commonService.getSectionName(this.overviewTab,'ENTITY_RISK');
        this.otherReferences.sectionId = this.commonService.getSectionId(this.overviewTab,'OTHER_REFERENCE_IDS');
        this.otherReferences.sectionName = this.commonService.getSectionName(this.overviewTab,'OTHER_REFERENCE_IDS');
        this.attachmentSectionDetails.sectionId = this.commonService.getSectionId(this.overviewTab,'ENTITY_ATTACHMENTS');
        this.attachmentSectionDetails.sectionName = this.commonService.getSectionName(this.overviewTab,'ENTITY_ATTACHMENTS')
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private getDataFromStore() {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) {
            return;
        }
        this.entityRisksList = ENTITY_DATA?.entityRisks;
        this.entityAttachmentList = ENTITY_DATA?.attachments ? ENTITY_DATA.attachments : [];
    }

    riskUpdated(risksList: EntityRisk[]): void {
        this.entityRisksList = deepCloneObject(risksList);
        this._dataStoreService.updateStore(['entityRisks'], {'entityRisks': this.entityRisksList});
    }

}
