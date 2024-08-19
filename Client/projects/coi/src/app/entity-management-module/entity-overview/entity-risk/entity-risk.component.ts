import { Component, Input } from '@angular/core';
import { hideModal, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntityRisk } from '../../shared/entity-interface';

@Component({
  selector: 'app-entity-risk',
  templateUrl: './entity-risk.component.html',
  styleUrls: ['./entity-risk.component.scss']
})
export class EntityRiskComponent {

    entityRisk: EntityRisk = new EntityRisk();
    entityRiskTypeOptions = 'ENTITY_RISK_TYPE#RISK_TYPE_CODE#true#true';
    entityRiskLevelOption = 'ENTITY_RISK_LEVEL#RISK_LEVEL_CODE#true#true'
    @Input() sectionName: any;
    @Input() sectionId: any;
    mandatoryList = new Map();

    addRisk(event) {
        openModal('addEntityRisk');
    }

    onRiskSelected(event) {
        if(event) {
            this.entityRisk.riskType = event.code;
        } else {
            this.entityRisk.riskType = null;
        }
    }

    onRiskLevelSelected(event) {
        if(event) {
            this.entityRisk.riskLevel = event.code;
        } else {
            this.entityRisk.riskLevel = null;
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
            this.clearRiskDetails();
        }
    }

    entityMandatoryValidation(): void {
        this.mandatoryList.clear();
        if(!this.entityRisk.riskLevel) {
            this.mandatoryList.set('riskLevel', 'Please select risk level.');
        }
        if(!this.entityRisk.riskType) {
            this.mandatoryList.set('riskType', 'Please select risk type.');
        }
        if(!this.entityRisk.riskDescription) {
            this.mandatoryList.set('riskDescription', 'Please enter risk description.');
        }
    }

}
