import { Component, Input } from '@angular/core';
import { hideModal, isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntityRisk } from '../../shared/entity-interface';
import { Subscription } from 'rxjs';
import { EntityOverviewService } from '../entity-overview.service';
import { EntityDataStoreService } from '../../entity-data-store.service';

@Component({
  selector: 'app-entity-risk',
  templateUrl: './entity-risk.component.html',
  styleUrls: ['./entity-risk.component.scss']
})
export class EntityRiskComponent {

    entityRisk: EntityRisk = new EntityRisk();
    entityRiskTypeOptions = 'EMPTY#EMPTY#false#false';
    entityRiskLevelOption = 'ENTITY_RISK_LEVEL#RISK_LEVEL_CODE#false#false';
    @Input() sectionName: any;
    @Input() sectionId: any;
    mandatoryList = new Map();
    $subscriptions: Subscription[] = [];
    entityId: any;
    entityRisks: any;
    entityRiskTypeList: any;

    addRisk(event) {
        openModal('addEntityRisk');
    }

    ngOnInit(){
        this.fetchRisk();
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    constructor(private _entityOverviewService: EntityOverviewService, private _dataStoreService: EntityDataStoreService) {}

    onRiskSelected(event) {
        if(event) {
            this.entityRisk.riskTypeCode = event[0].riskTypeCode;
        } else {
            this.entityRisk.riskTypeCode = null;
        }
    }

    fetchRisk() {
        this.$subscriptions.push(this._entityOverviewService.fetchRiskType().subscribe((data: any) => {
            if(data?.length) {
                this.entityRiskTypeList = data.filter(ele => ele.riskCategoryCode == 'EN');
                console.log(this.entityRiskTypeList);
            }
        }))
    }

    private getDataFromStore() {
        const entityData = this._dataStoreService.getData();
        if (isEmptyObject(entityData)) { return; }
        this.entityId = entityData?.entityDetails?.entityId;
        this.entityRisks = entityData.entityRisks;
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    onRiskLevelSelected(event) {
        if(event) {
            this.entityRisk.riskLevelCode = event[0].code;
        } else {
            this.entityRisk.riskLevelCode = null;
        }
    }

    clearRiskDetails() {
        this.mandatoryList.clear();
        this.entityRisk = new EntityRisk();
        hideModal('addEntityRisk');
    }

    addRiskDetails() {
        this.entityMandatoryValidation();
        if(!this.mandatoryList.size) {
            this.saveRisk();
        }
    }

    entityMandatoryValidation(): void {
        this.mandatoryList.clear();
        if(!this.entityRisk.riskLevelCode) {
            this.mandatoryList.set('riskLevel', 'Please select risk level.');
        }
        if(!this.entityRisk.riskTypeCode) {
            this.mandatoryList.set('riskType', 'Please select risk type.');
        }
        if(!this.entityRisk.description) {
            this.mandatoryList.set('riskDescription', 'Please enter risk description.');
        }
    }

    saveRisk() {
        this.entityRisk.entityId = this.entityId;
        this.$subscriptions.push(this._entityOverviewService.saveRisk(this.entityRisk).subscribe((data: any) => {
            this.clearRiskDetails();
        }))
    }

}
