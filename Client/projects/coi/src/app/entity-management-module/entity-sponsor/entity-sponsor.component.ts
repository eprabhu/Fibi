import { Component } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { EntityDataStoreService } from '../entity-data-store.service';
import { SponsorTabSection } from '../shared/entity-constants';
import { Subscription } from 'rxjs';
import { EntitySponsorService } from './entity-sponsor.service';
import { EntireEntityDetails, EntityAttachment, EntityDetails, EntityRisk } from '../shared/entity-interface';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { HTTP_ERROR_STATUS } from '../../app-constants';

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
    riskSubSectionId = 2610;
    attachmentSubSectionId = 2611;

    constructor(public commonService: CommonService, public dataStore: EntityDataStoreService,
        private _entitySponsorService: EntitySponsorService, private _dataStoreService: EntityDataStoreService
    ) { }

    ngOnInit() {
        window.scrollTo(0,0);
        this.overViewTab = SponsorTabSection;
        this.getDataFromStore();
        this.listenDataChangeFromStore();
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
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
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
