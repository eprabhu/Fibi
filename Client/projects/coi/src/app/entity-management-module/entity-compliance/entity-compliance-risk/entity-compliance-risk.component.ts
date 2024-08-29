import { Component, Input, OnInit } from '@angular/core';
import { EntityRisk } from '../../shared/entity-interface';
import { hideModal, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { EntityComplianceService } from '../entity-compliance.service';
import { EntityDataStoreService } from '../../entity-data-store.service';

@Component({
    selector: 'app-entity-compliance-risk',
    templateUrl: './entity-compliance-risk.component.html',
    styleUrls: ['./entity-compliance-risk.component.scss']
})
export class EntityComplianceRiskComponent implements OnInit {

    @Input() sectionName: any;
    @Input() sectionId: any;
    entityRisk: EntityRisk = new EntityRisk();
    entityRiskTypeOptions = 'ENTITY_RISK_TYPE#RISK_TYPE_CODE#true#true';
    entityRiskLevelOption = 'ENTITY_RISK_LEVEL#RISK_LEVEL_CODE#true#true'
    mandatoryList = new Map();
    $subscriptions: Subscription[] = [];
    entityRiskTypeList: any;
    selectedRiskTypeLookUpList: any[] = [];
    selectedLookUpList: any[] = [];
    entityRiskList: any;
    entityComplianceRisksDetails: any;
    entityDetails: any;

    constructor(private _entityComplianceService: EntityComplianceService,
        private _dataStoreService: EntityDataStoreService,
    ) { }

    ngOnInit() {
        this.fetchRiskTypes();
        this.fetchRiskData();
    }

    fetchRiskTypes() {
        this.entityRiskTypeList = this._dataStoreService.getFilterRiskByCode('CO');
    }

    fetchRiskData() {
        this.entityRiskList = this._entityComplianceService.entityCompliance;
    }

    addSponsorRisk(event) {
        openModal('addEntitySponsorRisk');
    }

    onRiskSelected(event) {
        if (event) {
            this.entityRisk.riskTypeCode = event.code;
        } else {
            this.entityRisk.riskTypeCode = null;
        }
    }

    onRiskLevelSelected(event) {
        if (event) {
            this.entityRisk.riskLevelCode = event.code;
        } else {
            this.entityRisk.riskLevelCode = null;
        }
    }

    clearRiskDetails() {
        this.mandatoryList.clear();
        this.entityRisk = new EntityRisk();
        hideModal('addEntitySponsorRisk');
    }

    addRiskDetails() {
        this.entityMandatoryValidation();
        if (!this.mandatoryList.size) {
            this.clearRiskDetails();
        }
    }

    entityMandatoryValidation(): boolean {
        this.mandatoryList.clear();
        if (!this.entityRisk.riskLevelCode) {
            this.mandatoryList.set('riskLevel', 'Please select risk level.');
        }
        if (!this.entityRisk.riskTypeCode) {
            this.mandatoryList.set('riskType', 'Please select risk type.');
        }
        if (!this.entityRisk.description) {
            this.mandatoryList.set('riskDescription', 'Please enter risk description.');
        }
        return this.mandatoryList.size > 0;
    }

    editRisk(risk: any , index: number) {
        // this.isEditIndex = index;
        // this.setSponsorRiskDetails(risk);
        // this.isEditRisk = true;
        openModal('addEntitySponsorRisk');
    }

    addOrUpdateRisk() {
        if (this.entityMandatoryValidation()) {
            // this.isEditRisk ?  this.UpdateSponsorRisk() : this.saveSponsorRisk();
            this.saveComplianceRisk();
        }
    }

    saveComplianceRisk() {
        this.entityRisk.entityId = this.entityDetails.entityId;
            this.$subscriptions.push(this._entityComplianceService.saveComplianceRisk(this.entityRisk).subscribe((data: any) => {
                let test:any = {};
                // test.riskLevelDescription = this.selectedRiskLevel?.description;
                // test.riskTypeDescription = this.selectedRiskType?.description;
                test.entityRiskId = data.entityRiskId;
                test.riskLevelCode = this.entityRisk?.riskLevelCode;
                test.riskTypeCode = this.entityRisk.riskTypeCode;
                test.description = this.entityRisk.description;
                this.entityComplianceRisksDetails.push(test);
                this._dataStoreService.updateStore(['entityComplianceRisksDetails'], { 'entityComplianceRisksDetails':  this.entityComplianceRisksDetails });
                this.clearRiskDetails();
        }))
    }

}
