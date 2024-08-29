import { Component } from '@angular/core';
import { EntityDataStoreService } from '../entity-data-store.service';
import { CommonService } from '../../common/services/common.service';
import { ComplianceTab } from '../shared/entity-constants';
import { EntityComplianceService } from './entity-compliance.service';
import { EntireEntityDetails, EntityDetails, EntityRisk, SubAwardOrganization, SubAwardOrganizationDetails } from '../shared/entity-interface';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { HTTP_ERROR_STATUS } from '../../app-constants';

@Component({
    selector: 'app-entity-compliance',
    templateUrl: './entity-compliance.component.html',
    styleUrls: ['./entity-compliance.component.scss'],
    providers: [EntityComplianceService]
})
export class EntityComplianceComponent {

    overViewTab: any;
    isLoading = true;
    entityDetails = new EntityDetails();
    $subscriptions: Subscription[] = [];
    complianceEntityRiskList: EntityRisk[] = [];

    constructor(public commonService: CommonService, private _dataStoreService: EntityDataStoreService, private _entityComplianceService: EntityComplianceService) { }

    ngOnInit() {
        window.scrollTo(0, 0);
        this.overViewTab = ComplianceTab;
        this.getDataFromStore();
        this.listenDataChangeFromStore();
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
            this._dataStoreService.dataEvent.subscribe((dependencies: string[] | 'ENTITY_RISK_TYPE') => {
                if (dependencies !==  'ENTITY_RISK_TYPE') {
                    this.getDataFromStore();
                }
            })
        );
    }

    private fetchEntityDetails(entityId: string | number): void {
        this.$subscriptions.push(this._entityComplianceService.fetchEntityComplianceDetails(entityId).subscribe((data: SubAwardOrganization) => {
            this.complianceEntityRiskList = data.entityRisks;
            this._entityComplianceService.entityCompliance = data;
            this.isLoading = false;
        }, (_error: any) => {
            this.isLoading = false;
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }


}
