import { Component, Input } from '@angular/core';
import { hideModal, isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntityRisk } from '../../shared/entity-interface';
import { Subscription } from 'rxjs';
import { EntityOverviewService } from '../entity-overview.service';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { COIModalConfig, ModalActionEvent } from '../../../shared-components/coi-modal/coi-modal.interface';
import { closeCommonModal, openCommonModal } from '../../../common/utilities/custom-utilities';
import { CommonService } from '../../../common/services/common.service';
import { HTTP_ERROR_STATUS } from '../../../app-constants';

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
    selectedRiskType: any;
    selectedRiskLevel: any;
    isSaving = false;
    CONFIRMATIN_MODAL_ID = 'risk-delete-confirm-modal'
    modalConfig = new COIModalConfig(this.CONFIRMATIN_MODAL_ID, 'Delete', 'Cancel');
    deleteRiskObj = null;
    isEditIndex: null | number = null;

    addRisk(event) {
        openModal('addEntityRisk');
    }

    ngOnInit(){
        this.fetchRisk();
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    constructor(private _entityOverviewService: EntityOverviewService, private _dataStoreService: EntityDataStoreService, private _commonService: CommonService) {}

    onRiskSelected(event) {
        if(event) {
            this.entityRisk.riskTypeCode = event[0].riskTypeCode;
            this.selectedRiskType = event[0];
        } else {
            this.entityRisk.riskTypeCode = null;
            this.selectedRiskType = null;
        }
    }

    fetchRisk() {
        this.$subscriptions.push(this._entityOverviewService.fetchRiskType().subscribe((data: any) => {
            if(data?.length) {
                this.entityRiskTypeList = data.filter(ele => ele.riskCategoryCode == 'EN');
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
            this.selectedRiskLevel = event[0];
        } else {
            this.entityRisk.riskLevelCode = null;
            this.selectedRiskLevel = null;
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
            let test:any = {};
            test.riskLevelDescription = this.selectedRiskLevel?.description;
            test.riskTypeDescription = this.selectedRiskType?.description;
            test.entityRiskId = data.entityRiskId;
            test.riskLevelCode = this.entityRisk.riskLevelCode;
            test.riskTypeCode = this.entityRisk.riskTypeCode;
            test.description = this.entityRisk.description;
            this.entityRisks.push(test);
            this._dataStoreService.updateStore(['entityRisks'], { 'entityRisks':  this.entityRisks });
            this.clearRiskDetails();
        }))
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
            this.$subscriptions.push(this._entityOverviewService.deleteRisk(this.deleteRiskObj.entityRiskId).subscribe((res: any) => {
                this.entityRisks.splice(this.isEditIndex, 1);
                this.deleteRiskObj = null;
                this.isSaving = false;
            }, err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                this.isSaving = false;
            }))
        }
    }

    confirmDeleteRisk(risk, index) {
        this.deleteRiskObj = risk;
        this.isEditIndex = index;
        openCommonModal(this.CONFIRMATIN_MODAL_ID);
    }

}
