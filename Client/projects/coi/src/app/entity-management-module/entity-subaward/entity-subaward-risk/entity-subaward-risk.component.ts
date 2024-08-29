import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { EntityRisk, EntityRiskModalDetails } from '../../shared/entity-interface';
import { hideModal, isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subject, Subscription } from 'rxjs';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { CommonService } from '../../../common/services/common.service';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { EntitySponsorService } from '../../entity-sponsor/entity-sponsor.service';
import { COIModalConfig, ModalActionEvent } from '../../../shared-components/coi-modal/coi-modal.interface';

@Component({
    selector: 'app-entity-subaward-risk',
    templateUrl: './entity-subaward-risk.component.html',
    styleUrls: ['./entity-subaward-risk.component.scss']
})
export class EntitySubawardRiskComponent implements OnInit, OnDestroy {

    @Input() sectionName: any;
    @Input() sectionId: any;
    entityRiskModalDetails = new EntityRiskModalDetails();
    entityRiskTypeOptions = 'ENTITY_RISK_TYPE#RISK_TYPE_CODE#false#false';
    entityRiskLevelOption = 'ENTITY_RISK_LEVEL#RISK_LEVEL_CODE#false#false'
    mandatoryList = new Map();
    ENTITY_RISK_ADD_UPDATE_MODAL_ID = 'entity-risk-add-update-modal';
    entityRiskModalConfig = new COIModalConfig(this.ENTITY_RISK_ADD_UPDATE_MODAL_ID, 'Delete', 'Cancel');

    entityDetails: any;
    entitySubAwardRisk: any;
    entityRiskTypeList: any[] = [];
    $subscriptions: Subscription[] = [];
    isSponsorDetailsFormChanged: boolean;
    autoSaveRO: any = {};
    $debounceEvent = new Subject<any>();

    constructor( private _dataStoreService: EntityDataStoreService,
        private _commonService: CommonService) { }

    ngOnInit() {
        window.scrollTo(0,0);
        this.fetchRisk();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private fetchRisk(): void {
        this.entityRiskTypeList = this._dataStoreService.getFilterRiskByCode('OR');
    }

    addSponsorRisk(event: any): void {
        openModal('addEntitySponsorRisk');
    }

    onRiskSelected(event: any[] | null): void {
        // this.entityRiskModalDetails.riskTypeCode = event ? event[0].riskTypeCode : null;
    }

    onRiskLevelSelected(event: any[] | null): void {
        // this.entityRiskModalDetails.riskLevelCode = event ? event[0].code : null;
    }

    clearRiskDetails(): void {
        this.mandatoryList.clear();
        // this.entityRiskModalDetails = new EntityRiskModalDetails();
        hideModal('addEntitySponsorRisk');
    }

    addRiskDetails(): void {
        this.entityMandatoryValidation();
    }

    entityMandatoryValidation(): boolean {
        this.mandatoryList.clear();
        // if (!this.entityRiskModalDetails.riskLevelCode) {
            this.mandatoryList.set('riskLevel', 'Please select risk level.');
        // }
        // if (!this.entityRiskModalDetails.riskTypeCode) {
            this.mandatoryList.set('riskType', 'Please select risk type.');
        // }
        // if (!this.entityRiskModalDetails.description) {
            this.mandatoryList.set('riskDescription', 'Please enter risk description.');
        // }
        return this.mandatoryList.size === 0;
    }

    setSponsorRiskDetails(risk: any){
        // this.entityRiskModalDetails.entityRiskId = risk.entityRiskId;
        // this.entityRiskModalDetails.riskTypeCode = risk.riskTypeCode;
        // this.entityRiskModalDetails.riskLevelCode = risk.riskLevelCode;
        // this.entityRiskModalDetails.description = risk.description;
        // this.entityRiskModalDetails.defaultRiskLevel = risk?.riskLevelDescription ? risk?.riskLevelDescription : risk?.riskLevel?.description;
        // this.entityRiskModalDetails.selectedRiskTypeLookUpList = [this.entityRiskTypeList.find((_risk: any) => risk?.riskTypeCode ? risk?.riskTypeCode : risk?.riskType?.riskTypeCode === _risk.riskTypeCode)];
        // this.entityRiskModalDetails.description = risk.description;
    }

    RiskModalActions(modalAction: ModalActionEvent): void {
        
    }
}
