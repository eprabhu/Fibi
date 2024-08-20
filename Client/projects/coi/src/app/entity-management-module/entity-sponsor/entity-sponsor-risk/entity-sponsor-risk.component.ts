import { Component, Input, OnInit } from '@angular/core';
import { hideModal, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntityRisk } from '../../shared/entity-interface';

@Component({
    selector: 'app-entity-sponsor-risk',
    templateUrl: './entity-sponsor-risk.component.html',
    styleUrls: ['./entity-sponsor-risk.component.scss']
})
export class EntitySponsorRiskComponent implements OnInit {

    entityRisk: EntityRisk = new EntityRisk();
    entityRiskTypeOptions = 'ENTITY_RISK_TYPE#RISK_TYPE_CODE#true#true';
    entityRiskLevelOption = 'ENTITY_RISK_LEVEL#RISK_LEVEL_CODE#true#true'
    @Input() sectionName: any;
    @Input() sectionId: any;
    mandatoryList = new Map();

    constructor() { }

    ngOnInit() {
    }

    addSponsorRisk(event) {
        openModal('addEntitySponsorRisk');
    }

    onRiskSelected(event) {
        if (event) {
            this.entityRisk.riskType = event.code;
        } else {
            this.entityRisk.riskType = null;
        }
    }

    onRiskLevelSelected(event) {
        if (event) {
            this.entityRisk.riskLevel = event.code;
        } else {
            this.entityRisk.riskLevel = null;
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
        if (!this.entityRisk.riskLevel) {
            this.mandatoryList.set('riskLevel', 'Please select risk level.');
        }
        if (!this.entityRisk.riskType) {
            this.mandatoryList.set('riskType', 'Please select risk type.');
        }
        if (!this.entityRisk.riskDescription) {
            this.mandatoryList.set('riskDescription', 'Please enter risk description.');
        }
    }
}
