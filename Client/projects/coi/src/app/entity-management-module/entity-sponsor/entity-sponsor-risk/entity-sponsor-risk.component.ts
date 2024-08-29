import { Component, Input, OnInit } from '@angular/core';
import { hideModal, isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntireEntityDetails, EntitySponsorRisk } from '../../shared/entity-interface';
import { Subscription } from 'rxjs';
import { EntitySponsorService } from '../entity-sponsor.service';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { COIModalConfig, ModalActionEvent } from '../../../shared-components/coi-modal/coi-modal.interface';
import { closeCommonModal, openCommonModal } from '../../../common/utilities/custom-utilities';
import { CommonService } from '../../../common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';

@Component({
    selector: 'app-entity-sponsor-risk',
    templateUrl: './entity-sponsor-risk.component.html',
    styleUrls: ['./entity-sponsor-risk.component.scss']
})
export class EntitySponsorRiskComponent implements OnInit {

    entitySponsorRiskRO: EntitySponsorRisk = new EntitySponsorRisk();
    entitySponsorRiskTypeOptions = 'ENTITY_RISK_TYPE#RISK_TYPE_CODE#false#false';
    entitySponsorRiskLevelOption = 'ENTITY_RISK_LEVEL#RISK_LEVEL_CODE#false#false';
    @Input() sectionName: any;
    @Input() sectionId: any;
    // @Input() entitySponsorDetails: any;
    mandatoryList = new Map();
    $subscriptions: Subscription[] = [];
    entitySponsorRisksDetails: any;
    entityRiskTypeList: any;
    selectedRiskType: any;
    selectedRiskLevel: any;
    CONFIRMATIN_MODAL_ID = 'risk-delete-confirm-modal'
    modalConfig = new COIModalConfig(this.CONFIRMATIN_MODAL_ID, 'Delete', 'Cancel');
    isSaving = false;
    deleteRiskObj = null;
    isEditIndex: null | number = null;
    isEditRisk = false;
    defaultValue: string;
    selectedLookUpList: any[] = [];
    selectedRiskTypeLookUpList: any[] = [];
    entityDetails: any;
    entityRegistrationDefaultValue = '';
    entityRegistrationDefaultValue2 = '';

    constructor(private _entitySponsorService: EntitySponsorService,
        private _dataStoreService: EntityDataStoreService,
        private _commonService: CommonService) { }

    ngOnInit() {
        window.scrollTo(0,0);
        this.fetchRisk();
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    fetchRisk() {
        this.entityRiskTypeList = this._dataStoreService.getFilterRiskByCode('SP');
    }

    private getDataFromStore() {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        if (this.entityDetails?.entityId != ENTITY_DATA?.entityDetails?.entityId) {
            this.fetchEntityDetails(ENTITY_DATA?.entityDetails?.entityId);
        }
        this.entityDetails = ENTITY_DATA.entityDetails;
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[] | 'ENTITY_RISK_TYPE') => {
                if (dependencies !==  'ENTITY_RISK_TYPE') {
                    this.getDataFromStore();
                } else {
                    this.fetchRisk();
                }
            })
        );
    }

    addSponsorRisk(event) {
        openModal('addEntitySponsorRisk');
    }

    onRiskSelected(event) {
        if (event) {
            this.entitySponsorRiskRO.riskTypeCode = event[0].riskTypeCode;
            this.selectedRiskType = event[0];

        } else {
            this.entitySponsorRiskRO.riskTypeCode = null;
            this.selectedRiskType = null;

        }
    }

    onRiskLevelSelected(event) {
        if (event) {
            this.entitySponsorRiskRO.riskLevelCode = event[0].code;
            this.selectedRiskLevel = event[0];

        } else {
            this.entitySponsorRiskRO.riskLevelCode = null;
            this.selectedRiskLevel = null;
        }
    }

    clearRiskDetails() {
        hideModal('addEntitySponsorRisk');
        setTimeout(() => {
            this.isEditRisk = false;
            this.mandatoryList.clear();
            this.entitySponsorRiskRO = new EntitySponsorRisk();
            this.defaultValue = '';
            this.selectedLookUpList = [];
            this.selectedRiskTypeLookUpList = [];
        }, 200);
    }

    addOrUpdateRisk() {
        if (this.entityMandatoryValidation()) {
            this.isEditRisk ?  this.UpdateSponsorRisk() : this.saveSponsorRisk();
        }
    }

    entityMandatoryValidation(): boolean {
        this.mandatoryList.clear();
        if (!this.entitySponsorRiskRO.riskLevelCode) {
            this.mandatoryList.set('riskLevel', 'Please select risk level.');
        }
        if (!this.entitySponsorRiskRO.riskTypeCode) {
            this.mandatoryList.set('riskType', 'Please select risk type.');
        }
        if (!this.entitySponsorRiskRO.description) {
            this.mandatoryList.set('riskDescription', 'Please enter risk description.');
        }

        return this.mandatoryList.size === 0
    }

    saveSponsorRisk() {
        this.entitySponsorRiskRO.entityId = this.entityDetails.entityId;
            this.$subscriptions.push(this._entitySponsorService.saveSponsorRisk(this.entitySponsorRiskRO).subscribe((data: any) => {
                let test:any = {};
                test.riskLevelDescription = this.selectedRiskLevel?.description;
                test.riskTypeDescription = this.selectedRiskType?.description;
                test.entityRiskId = data.entityRiskId;
                test.riskLevelCode = this.entitySponsorRiskRO?.riskLevelCode;
                test.riskTypeCode = this.entitySponsorRiskRO.riskTypeCode;
                test.description = this.entitySponsorRiskRO.description;
                this.entitySponsorRisksDetails.push(test);
                this._dataStoreService.updateStore(['entitySponsorRisksDetails'], { 'entitySponsorRisksDetails':  this.entitySponsorRisksDetails });
                this.clearRiskDetails();
        }))
    }

    UpdateSponsorRisk(){
        this.$subscriptions.push(this._entitySponsorService.updateSponsorRisk(this.entitySponsorRiskRO).subscribe((data: any) => {
            this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Risk updated successfully');
            if (data) {
                if (this.selectedRiskLevel?.description) {
                    this.entitySponsorRisksDetails[this.isEditIndex].riskLevel.description = 
                    this.entitySponsorRisksDetails[this.isEditIndex].description = this.selectedRiskLevel.description;
                }

                if (this.selectedRiskType?.description) {
                    this.entitySponsorRisksDetails[this.isEditIndex].riskType.description = 
                    this.entitySponsorRisksDetails[this.isEditIndex].description = this.selectedRiskType.description;
                }
                
                this.entitySponsorRisksDetails[this.isEditIndex].entityRiskId = this.entitySponsorRiskRO.entityRiskId;
                this.entitySponsorRisksDetails[this.isEditIndex].riskLevelCode = this.entitySponsorRiskRO.riskLevelCode;
                this.entitySponsorRisksDetails[this.isEditIndex].riskTypeCode = this.entitySponsorRiskRO.riskTypeCode;
                this.entitySponsorRisksDetails[this.isEditIndex].description = this.entitySponsorRiskRO.description;
                this._dataStoreService.updateStore(['entitySponsorRisksDetails'], { 'entitySponsorRisksDetails': this.entitySponsorRisksDetails });   
            }
            this.clearRiskDetails();
            
        }))
    }

    private fetchEntityDetails(entityId: string | number){
        this.entitySponsorRisksDetails = this._entitySponsorService.entitySponsorDetails.entityRisks;
    }

    confirmDeleteRisk(risk, index) {
        this.deleteRiskObj = risk;
        this.isEditIndex = index;
        openCommonModal(this.CONFIRMATIN_MODAL_ID);
    }

    postConfirmation(modalAction: ModalActionEvent) {
        if(modalAction.action == 'PRIMARY_BTN') {
            this.deleteRisk();
        }
        closeCommonModal(this.CONFIRMATIN_MODAL_ID);
    }

    deleteRisk() {
        if(!this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(this._entitySponsorService.deleteRisk(this.deleteRiskObj.entityRiskId).subscribe((res: any) => {
                this.entitySponsorRisksDetails.splice(this.isEditIndex, 1);
                this.deleteRiskObj = null;
                this.isSaving = false;
            }, err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                this.isSaving = false;
            }))
            this.clearRiskDetails();
        }
    }

    editRisk(risk: any , index: number) {
        this.isEditIndex = index;
        this.setSponsorRiskDetails(risk);
        this.isEditRisk = true;
        openModal('addEntitySponsorRisk');
    }

    setSponsorRiskDetails(risk: any){
        this.entitySponsorRiskRO.entityRiskId = risk.entityRiskId;
        this.entitySponsorRiskRO.riskTypeCode = risk.riskTypeCode;
        this.entitySponsorRiskRO.riskLevelCode = risk.riskLevelCode;
        this.entitySponsorRiskRO.description = risk.description;
        this.defaultValue = risk?.riskLevelDescription ? risk?.riskLevelDescription : risk?.riskLevel?.description;
        this.selectedRiskTypeLookUpList = [this.entityRiskTypeList.find((_risk: any) => risk?.riskTypeCode ? risk?.riskTypeCode : risk?.riskType?.riskTypeCode === _risk.riskTypeCode)];
        this.entitySponsorRiskRO.description = risk.description;
    }
}
