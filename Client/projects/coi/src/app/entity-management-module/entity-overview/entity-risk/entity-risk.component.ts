import {Component, Input, OnDestroy, OnInit} from '@angular/core';
import {
    deepCloneObject,
    hideModal,
    isEmptyObject,
    openModal
} from 'projects/fibi/src/app/common/utilities/custom-utilities';
import {EntityRisk} from '../../shared/entity-interface';
import {Subscription} from 'rxjs';
import {EntityOverviewService} from '../entity-overview.service';
import {EntityDataStoreService} from '../../entity-data-store.service';
import {COIModalConfig, ModalActionEvent} from '../../../shared-components/coi-modal/coi-modal.interface';
import {closeCommonModal, openCommonModal} from '../../../common/utilities/custom-utilities';
import {CommonService} from '../../../common/services/common.service';
import {HTTP_ERROR_STATUS} from '../../../app-constants';
import {subscriptionHandler} from "../../../../../../fibi/src/app/common/utilities/subscription-handler";

@Component({
    selector: 'app-entity-risk',
    templateUrl: './entity-risk.component.html',
    styleUrls: ['./entity-risk.component.scss']
})
export class EntityRiskComponent implements OnInit, OnDestroy {

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
    CONFIRMATION_MODAL_ID = 'risk-delete-confirm-modal';
    modalConfig = new COIModalConfig(this.CONFIRMATION_MODAL_ID, 'Delete', 'Cancel');
    selectedRiskObj = null;
    isEditIndex: null | number = null;
    isEditMode = false;
    defaultRiskType = '';
    defaultRiskLevel = '';
    selectedLookupType = [];
    selectedLookupLevel = [];

    addRisk(event) {
        openModal('addEntityRisk');
    }

    ngOnInit() {
        this.fetchRisk();
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    constructor(private _entityOverviewService: EntityOverviewService, private _dataStoreService: EntityDataStoreService, private _commonService: CommonService) {
    }

    onRiskSelected(event) {
        if (event && event.length) {
            this.entityRisk.riskTypeCode = event[0].riskTypeCode;
            this.selectedRiskType = event[0];
        } else {
            this.entityRisk.riskTypeCode = null;
            this.selectedRiskType = null;
        }
    }

    fetchRisk() {
        this.$subscriptions.push(this._entityOverviewService.fetchRiskType().subscribe((data: any) => {
            if (data?.length) {
                this.entityRiskTypeList = data.map(ele => {
                    const obj = ele;
                    delete obj.riskCategoryCode;
                    return obj;
                });
            }
        }));
    }

    private getDataFromStore() {
        const entityData = this._dataStoreService.getData();
        if (isEmptyObject(entityData)) {
            return;
        }
        this.entityId = entityData?.entityDetails?.entityId;
        this.entityRisks = entityData.entityRisks;
        this.isEditMode = this._dataStoreService.getEditMode();
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    onRiskLevelSelected(event) {
        if (event && event.length) {
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
        this.isEditIndex = null;
        this.selectedRiskObj = null;
        this.defaultRiskLevel = null;
        this.defaultRiskType = null;
        this.selectedLookupLevel = [];
        this.selectedLookupType = [];
        hideModal('addEntityRisk');
    }

    performRiskAction() {
        if (this.entityMandatoryValidation()) {
            this.isEditIndex != null ? this.editRiskDetails() : this.saveRisk();
        }
    }

    entityMandatoryValidation() {
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
        return !this.mandatoryList.size;
    }

    saveRisk() {
        this.entityRisk.entityId = this.entityId;
        this.$subscriptions.push(this._entityOverviewService.saveRisk(this.entityRisk).subscribe((data: any) => {
            let test: any = {};
            test.riskLevelDescription = this.selectedRiskLevel?.description;
            test.riskTypeDescription = this.selectedRiskType?.description;
            test.entityRiskId = data.entityRiskId;
            test.riskLevelCode = this.entityRisk.riskLevelCode;
            test.riskTypeCode = this.entityRisk.riskTypeCode;
            test.description = this.entityRisk.description;
            this.entityRisks.push(test);
            this.updateDataStore();
            this.clearRiskDetails();
        }));
    }

    postConfirmation(modalAction: ModalActionEvent) {
        if (modalAction.action == 'PRIMARY_BTN') {
            this.deleteRisk();
        }
        closeCommonModal(this.CONFIRMATION_MODAL_ID);
    }

    deleteRisk() {
        if (!this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(this._entityOverviewService.deleteRisk(this.selectedRiskObj.entityRiskId).subscribe((res: any) => {
                this.entityRisks.splice(this.isEditIndex, 1);
                this.updateDataStore();
                this.selectedRiskObj = null;
                this.isEditIndex = null;
                this.isSaving = false;
            }, err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                this.isSaving = false;
            }))
        }
    }

    confirmDeleteRisk(risk, index) {
        this.selectedRiskObj = risk;
        this.isEditIndex = index;
        openCommonModal(this.CONFIRMATION_MODAL_ID);
    }

    editRisk(risk, index) {
        this.isEditIndex = index;
        this.selectedRiskObj = risk;
        this.setEditObj(risk);
        openModal('addEntityRisk');
    }

    setEditObj(risk) {
        this.defaultRiskLevel = risk.riskLevelDescription || risk.riskLevel.description;
        this.defaultRiskType = risk.riskTypeDescription || risk.riskType.description;
        this.entityRisk.riskTypeCode = risk.riskTypeCode;
        this.entityRisk.riskLevelCode = risk.riskLevelCode;
        this.entityRisk.entityId = risk.entityId;
        this.entityRisk.entityRiskId = risk.entityRiskId;
        this.entityRisk.description = risk.description;
    }

    editRiskDetails() {
        if (!this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(this._entityOverviewService.updateRisk(this.entityRisk).subscribe((data: any) => {
                this.setRiskLevel();
                this.setRiskType();
                this.entityRisks[this.isEditIndex].description = this.entityRisk.description;
                this.updateDataStore();
                this.clearRiskDetails();
                this.isSaving = false;
            }, _err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                this.isSaving = false;
            }));
        }
    }

    setRiskLevel() {
        const currentRisk = this.entityRisks[this.isEditIndex];
        const selectedLevel = this.selectedRiskLevel;
        if (selectedLevel?.description || currentRisk?.riskLevelDescription) {
            currentRisk.riskLevelDescription = selectedLevel.description || currentRisk.riskLevelDescription;
        }
        if (selectedLevel || currentRisk?.riskLevel) {
            currentRisk.riskLevel = selectedLevel || currentRisk.riskLevel;
        }
        currentRisk.riskLevelCode =  this.entityRisk.riskLevelCode;
    }

    setRiskType() {
        const currentRisk = this.entityRisks[this.isEditIndex];
        const selectedType = this.selectedRiskType;
        if (selectedType?.description || currentRisk?.riskTypeDescription) {
            currentRisk.riskTypeDescription = selectedType.description || currentRisk.riskTypeDescription;
        }
        if (selectedType || currentRisk?.riskType) {
            currentRisk.riskType = selectedType || currentRisk.riskType;
        }
        currentRisk.riskTypeCode = this.entityRisk.riskTypeCode;
    }

    updateDataStore() {
        this._dataStoreService.updateStore(['entityRisks'], {'entityRisks': this.entityRisks});
    }
}
