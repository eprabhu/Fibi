import { Component } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { EntityDataStoreService } from '../entity-data-store.service';
import {
    SPONSOR_QUESTIONNAIRE_SUB_SECTION_ID,
    SponsorTabSection
} from '../shared/entity-constants';
import { Subscription } from 'rxjs';
import { EntitySponsorService } from './entity-sponsor.service';
import { DataStoreEvent, EntireEntityDetails, EntityAttachment, EntityDetails, EntityRisk, EntitySectionDetails } from '../shared/entity-interface';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';

@Component({
    selector: 'app-entity-sponsor',
    templateUrl: './entity-sponsor.component.html',
    styleUrls: ['./entity-sponsor.component.scss'],
    providers: [EntitySponsorService]
})
export class EntitySponsorComponent {

    overViewTab: any;
    entityDetails = new EntityDetails();
    $subscriptions: Subscription[] = [];
    entitySponsorRiskList: EntityRisk[] = [];
    entitySponsorAttachmentList: EntityAttachment[] = [];
    isLoading = true;
    sponsorDetails = new EntitySectionDetails();
    riskSectionDetails = new EntitySectionDetails();
    attachmentSectionDetails = new EntitySectionDetails();
    questionnaireSectionDetails = new EntitySectionDetails();

    constructor(public commonService: CommonService, public dataStore: EntityDataStoreService,
        private _entitySponsorService: EntitySponsorService, private _dataStoreService: EntityDataStoreService
    ) { }

    ngOnInit() {
        window.scrollTo(0,0);
        this.overViewTab = SponsorTabSection;
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.setSectionIdAndName();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    private setSectionIdAndName(){
        this.riskSectionDetails.subSectionId = 2610;
        this.attachmentSectionDetails.subSectionId = 2611;
        this.sponsorDetails.sectionId = this.commonService.getSectionId(this.overViewTab,'SPONSOR_DETAILS');
        this.sponsorDetails.sectionName = this.commonService.getSectionName(this.overViewTab,'SPONSOR_DETAILS');
        this.riskSectionDetails.sectionId = this.commonService.getSectionId(this.overViewTab,'SPONSOR_RISK');
        this.riskSectionDetails.sectionName = this.commonService.getSectionName(this.overViewTab,'SPONSOR_RISK');
        this.attachmentSectionDetails.sectionId = this.commonService.getSectionId(this.overViewTab,'SPONSOR_ATTACHMENTS');
        this.attachmentSectionDetails.sectionName = this.commonService.getSectionName(this.overViewTab,'SPONSOR_ATTACHMENTS');
        this.questionnaireSectionDetails.sectionId = this.commonService.getSectionId(this.overViewTab, 'SPONSOR_QUESTIONNAIRE');
        this.questionnaireSectionDetails.sectionName = this.commonService.getSectionName(this.overViewTab, 'SPONSOR_QUESTIONNAIRE');
        this.questionnaireSectionDetails.subSectionId = this.commonService.getSubSectionId(this.overViewTab, 'SPONSOR_QUESTIONNAIRE');
    }

    private getDataFromStore(): void {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        if (this.entityDetails?.entityId != ENTITY_DATA?.entityDetails?.entityId) {
            this.fetchEntitySponsorDetails(ENTITY_DATA?.entityDetails?.entityId);
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

    fetchEntitySponsorDetails(entityId: string | number): void{
        this.$subscriptions.push(this._entitySponsorService.fetchEntitySponsorDetails(entityId).subscribe((data: any)=>{
            this._entitySponsorService.entitySponsorDetails = data;
            this.entitySponsorRiskList = data?.entityRisks ? data.entityRisks : [];
            this.entitySponsorAttachmentList = data?.attachments ? data.attachments : [];
            this.isLoading = false;
        }, (_error: any) => {
            this.isLoading = false;
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }

}
