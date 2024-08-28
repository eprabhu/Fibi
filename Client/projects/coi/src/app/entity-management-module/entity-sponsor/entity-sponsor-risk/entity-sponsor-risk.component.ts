import { Component, Input, OnInit } from '@angular/core';
import { hideModal, isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntitySponsorRisk } from '../../shared/entity-interface';
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

    entitySponsorRisk: EntitySponsorRisk = new EntitySponsorRisk();
    entitySponsorRiskTypeOptions = 'ENTITY_RISK_TYPE#RISK_TYPE_CODE#false#false';
    entitySponsorRiskLevelOption = 'ENTITY_RISK_LEVEL#RISK_LEVEL_CODE#false#false';
    @Input() sectionName: any;
    @Input() sectionId: any;
    mandatoryList = new Map();
    $subscriptions: Subscription[] = [];
    entityId: any;
    entitySponsorRisks: any;
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
        this.fetchEntityDetails();
        this.listenDataChangeFromStore();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
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
        hideModal('addEntitySponsorRisk');
        setTimeout(() => {
            this.isEditRisk = false;
            this.mandatoryList.clear();
            this.entitySponsorRisk = new EntitySponsorRisk();
            this.defaultValue = '';
            this.selectedLookUpList = [];
            this.selectedRiskTypeLookUpList = [];
        }, 200);
    }

    addOrUpdateRisk() {
        if(!this.isEditRisk){
            this.entityMandatoryValidation();
            if (!this.mandatoryList.size) {
                this.saveSponsorRisk()
            }
        }
        else{
            this.entityMandatoryValidation();
            if (!this.mandatoryList.size) {
            this.UpdateSponsorRisk();
            }
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
                test.riskLevelCode = this.entitySponsorRisk?.riskLevelCode;
                test.riskTypeCode = this.entitySponsorRisk.riskTypeCode;
                test.description = this.entitySponsorRisk.description;
                this.entitySponsorRisks.push(test);
                this._dataStoreService.updateStore(['entitySponsorRisks'], { 'entitySponsorRisks':  this.entitySponsorRisks });
                this.clearRiskDetails();
        }))
    }

    UpdateSponsorRisk(){
        this.$subscriptions.push(this._entitySponsorService.updateSponsorRisk(this.entitySponsorRisk).subscribe((data: any) => {
            this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Risk updated successfully');
            if (data) {
                console.log("data is ", data);
                
                console.log("desc", this.selectedRiskLevel?.description);
                
                this.entitySponsorRisks[this.isEditIndex].riskLevelDescription = this.selectedRiskLevel?.description;

                console.log("test" ,  this.entitySponsorRisks[this.isEditIndex].riskLevelDescription);
                
                this.entitySponsorRisks[this.isEditIndex].riskTypeDescription = this.selectedRiskType?.description;
                this.entitySponsorRisks[this.isEditIndex].entityRiskId = this.entitySponsorRisk.entityRiskId;
                this.entitySponsorRisks[this.isEditIndex].riskLevelCode = this.entitySponsorRisk.riskLevelCode;
                this.entitySponsorRisks[this.isEditIndex].riskTypeCode = this.entitySponsorRisk.riskTypeCode;
                this.entitySponsorRisks[this.isEditIndex].description = this.entitySponsorRisk.description;
                this._dataStoreService.updateStore(['entitySponsorRisks'], { 'entitySponsorRisks': this.entitySponsorRisks });   
            }
            this.clearRiskDetails();
            
        }))
    }

    fetchEntityDetails(){
        this.$subscriptions.push(this._entitySponsorService.fetchEntityDetails(this.entityId).subscribe((data: any)=>{
            this.entitySponsorRisks = data.entityRisks;
            this.entityDetails = data;    
        }));
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
                this.entitySponsorRisks.splice(this.isEditIndex, 1);
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
        this.entitySponsorRisk.entityRiskId = risk.entityRiskId;
        this.entitySponsorRisk.riskTypeCode = risk.riskTypeCode;
        this.entitySponsorRisk.riskLevelCode = risk.riskLevelCode;
        this.entitySponsorRisk.description = risk.description;
        this.defaultValue = risk?.riskLevelDescription ? risk?.riskLevelDescription : risk?.riskLevel?.description;
        this.selectedRiskTypeLookUpList = [this.entityRiskTypeList.find((_risk: any) => risk?.riskTypeCode ? risk?.riskTypeCode : risk?.riskType?.riskTypeCode === _risk.riskTypeCode)];
        this.entitySponsorRisk.description = risk.description;
    }
}
