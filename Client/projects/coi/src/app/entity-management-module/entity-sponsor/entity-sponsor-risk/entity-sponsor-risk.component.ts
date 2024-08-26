import { Component, Input, OnInit } from '@angular/core';
import { hideModal, isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntitySponsorRisk } from '../../shared/entity-interface';
import { Subscription } from 'rxjs';
import { EntitySponsorService } from '../entity-sponsor.service';
import { EntityDataStoreService } from '../../entity-data-store.service';

@Component({
    selector: 'app-entity-sponsor-risk',
    templateUrl: './entity-sponsor-risk.component.html',
    styleUrls: ['./entity-sponsor-risk.component.scss']
})
export class EntitySponsorRiskComponent implements OnInit {

    entitySponsorRisk: EntitySponsorRisk = new EntitySponsorRisk();
    entitySponsorRiskTypeOptions = 'ENTITY_RISK_TYPE#RISK_TYPE_CODE#false#false';
    entitySponsorRiskLevelOption = 'ENTITY_RISK_LEVEL#RISK_LEVEL_CODE#false#false'
    @Input() sectionName: any;
    @Input() sectionId: any;
    mandatoryList = new Map();
    $subscriptions: Subscription[] = [];
    entityId: any;
    entitySponsorRisks: any;
    entityRiskTypeList: any;
    selectedRiskType: any;
    selectedRiskLevel: any;

    constructor(private _entitySponsorService: EntitySponsorService, private _dataStoreService: EntityDataStoreService) { }

    ngOnInit() {
        window.scrollTo(0,0);
        this.fetchRisk();
        this.getDataFromStore();
        this.fetchEntityDetails();
        this.listenDataChangeFromStore();
    }

    fetchRisk() {
        this.$subscriptions.push(this._entitySponsorService.fetchRiskType().subscribe((data: any) => {
            if(data?.length) {
                this.entityRiskTypeList = data.filter(ele => ele.riskCategoryCode == 'SP');
            }
        }))
    }

    private getDataFromStore() {
        const entityData = this._dataStoreService.getData();
        if (isEmptyObject(entityData)) { return; }
        this.entityId = entityData?.entityDetails?.entityId;
        // this.entityRisks = entityData.entityRisks;
        console.log("data" , entityData.entityRisks);
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    addSponsorRisk(event) {
        openModal('addEntitySponsorRisk');
    }

    onRiskSelected(event) {
        if (event) {
            this.entitySponsorRisk.riskTypeCode = event[0].riskTypeCode;
            this.selectedRiskType = event[0];

        } else {
            this.entitySponsorRisk.riskTypeCode = null;
            this.selectedRiskType = null;

        }
    }

    onRiskLevelSelected(event) {
        if (event) {
            this.entitySponsorRisk.riskLevelCode = event[0].code;
            this.selectedRiskLevel = event[0];

        } else {
            this.entitySponsorRisk.riskLevelCode = null;
            this.selectedRiskLevel = null;

        }
    }

    clearRiskDetails() {
        this.mandatoryList.clear();
        this.entitySponsorRisk = new EntitySponsorRisk();
        hideModal('addEntitySponsorRisk');
    }

    addRiskDetails() {
        this.entityMandatoryValidation();
        if (!this.mandatoryList.size) {
            this.saveSponsorRisk()
        }
    }

    entityMandatoryValidation(): void {
        this.mandatoryList.clear();
        if (!this.entitySponsorRisk.riskLevelCode) {
            this.mandatoryList.set('riskLevel', 'Please select risk level.');
        }
        if (!this.entitySponsorRisk.riskTypeCode) {
            this.mandatoryList.set('riskType', 'Please select risk type.');
        }
        if (!this.entitySponsorRisk.description) {
            this.mandatoryList.set('riskDescription', 'Please enter risk description.');
        }
    }

    saveSponsorRisk() {
        this.entitySponsorRisk.entityId = this.entityId;
        this.$subscriptions.push(this._entitySponsorService.saveSponsorRisk(this.entitySponsorRisk).subscribe((data: any) => {
            let test:any = {};
            test.riskLevelDescription = this.selectedRiskLevel?.description;
            test.riskTypeDescription = this.selectedRiskType?.description;
            test.entityRiskId = data.entityRiskId;
            test.riskLevelCode = this.entitySponsorRisk.riskLevelCode;
            test.riskTypeCode = this.entitySponsorRisk.riskTypeCode;
            test.description = this.entitySponsorRisk.description;
            this.entitySponsorRisks.push(test);
            this._dataStoreService.updateStore(['entitySponsorRisks'], { 'entitySponsorRisks':  this.entitySponsorRisks });
            this.clearRiskDetails();
        }))
    }

    fetchEntityDetails(){
        this.$subscriptions.push(this._entitySponsorService.fetchEntityDetails(this.entityId).subscribe((data: any)=>{
            this.entitySponsorRisks = data.entityRisks;     
        }));
    }
}
