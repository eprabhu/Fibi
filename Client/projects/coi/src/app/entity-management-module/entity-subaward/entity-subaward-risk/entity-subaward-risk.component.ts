import { Component, Input, OnInit } from '@angular/core';
import { EntityRisk } from '../../shared/entity-interface';
import { hideModal, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';

@Component({
    selector: 'app-entity-subaward-risk',
    templateUrl: './entity-subaward-risk.component.html',
    styleUrls: ['./entity-subaward-risk.component.scss']
})
export class EntitySubawardRiskComponent implements OnInit {

    @Input() sectionName: any;
    @Input() sectionId: any;
    entityRisk: EntityRisk = new EntityRisk();
    entityRiskTypeOptions = 'ENTITY_RISK_TYPE#RISK_TYPE_CODE#true#true';
    entityRiskLevelOption = 'ENTITY_RISK_LEVEL#RISK_LEVEL_CODE#true#true'
    mandatoryList = new Map();

    constructor() { }

    ngOnInit() {
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

    entityMandatoryValidation(): void {
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
    }
}
