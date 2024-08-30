import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { EntireEntityDetails, EntityDetails, EntityRisk, EntityRiskProxyController, EntityRiskCategoryCode, EntityRiskModalDetails, EntityRiskRO, RiskType, RiskLevel } from '../../shared/entity-interface';
import { deepCloneObject, isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { CommonService } from '../../../common/services/common.service';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { COIModalConfig, ModalActionEvent } from '../../../shared-components/coi-modal/coi-modal.interface';
import { EntityRiskSectionService } from './entity-risk-section.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { closeCommonModal, openCommonModal } from '../../../common/utilities/custom-utilities';

@Component({
    selector: 'app-entity-risk-section',
    templateUrl: './entity-risk-section.component.html',
    styleUrls: ['./entity-risk-section.component.scss'],
    providers: [EntityRiskSectionService]
})
export class EntityRiskSectionComponent implements OnInit, OnDestroy {

    @Input() sectionId: any;
    @Input() sectionName: any;
    @Input() subSectionId: any;
    @Input() entityRiskList: EntityRisk[];
    @Input() riskCategoryCode: EntityRiskCategoryCode;

    isEditRisk = false;
    editIndex: number = -1;
    isOpenRiskModal = false;
    mandatoryList = new Map();
    entityRiskTypeList: RiskType[] = [];
    entityRiskLevelList: RiskLevel[] = [];
    entityDetails = new EntityDetails();
    $subscriptions: Subscription[] = [];
    entityRiskModalDetails = new EntityRiskModalDetails();
    entityRiskTypeOptions = 'ENTITY_RISK_TYPE#RISK_TYPE_CODE#false#false';
    entityRiskLevelOption = 'ENTITY_RISK_LEVEL#RISK_LEVEL_CODE#false#false'
    ENTITY_RISK_ADD_UPDATE_MODAL_ID: string = 'entity-risk-add-update-modal';
    entityRiskModalConfig = new COIModalConfig(this.ENTITY_RISK_ADD_UPDATE_MODAL_ID, 'Add Risk', 'Cancel', 'lg');

    constructor(private _dataStoreService: EntityDataStoreService,
        private _commonService: CommonService, private _entityRiskSectionService: EntityRiskSectionService) { }

    ngOnInit(): void {
        this.listenDataChangeFromStore();
        this.getDataFromStore();
        this.fetchRisk();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private getDataFromStore(): void {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        this.entityDetails = ENTITY_DATA.entityDetails;
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            }));
    }

    private fetchRisk(): void {
        this.fetchRiskTypes();
        this.fetchRiskLevels();
    }

    private fetchRiskLevels(): void {
        this.$subscriptions.push(
            this._entityRiskSectionService.fetchRiskLevels(this.getSectionCode())
                .subscribe((data: any) => {
                    this.entityRiskLevelList = data.map(item => item.entityRiskLevel);
                }, err => {
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                }));
    }

    private fetchRiskTypes(): void {
        this.$subscriptions.push(
            this._entityRiskSectionService.fetchRiskTypes(this.riskCategoryCode)
                .subscribe((data: RiskType[]) => {
                    this.entityRiskTypeList = data;
                }, err => {
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                }));
    }

    getSectionCode(): string {
        switch (this.riskCategoryCode) {
            case 'CO': return '4'; // Compliance - Attachment
            case 'OR': return '3'; // Organization - Attachment
            case 'SP': return '2'; // Sponsor - Attachment
            case 'EN': return '1'; // General - Attachment
            default: return '';
        }
    }


    private clearRiskDetails(): void {
        closeCommonModal(this.ENTITY_RISK_ADD_UPDATE_MODAL_ID);
        setTimeout(() => {
            this.mandatoryList.clear();
            this.entityRiskModalDetails = new EntityRiskModalDetails();
            this.entityRiskModalDetails.entityRisk.description = '';
        }, 200);
    }

    private addOrUpdateRisk(): void {
        if (this.entityMandatoryValidation()) {
            this.isEditRisk ? this.updateEntityRisk() : this.saveEntityRisk();
        }
    }

    private saveEntityRisk(): void {
        this.entityRiskModalDetails.entityRisk.entityId = this.entityDetails.entityId;
        this.$subscriptions.push(this._entityRiskSectionService.saveEntityRisk(this.getEntityRO(), this.getProxyController()).subscribe((data: any) => {
            this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Risk added successfully.');
            this.addNewRiskDetails(data.entityRiskId);
            this.clearRiskDetails();
        }))
    }

    private addNewRiskDetails(entityRiskId: number): void {
        this.entityRiskModalDetails.entityRisk.entityRiskId = entityRiskId;
        const NEW_ENTITY_RISK = deepCloneObject(this.entityRiskModalDetails.entityRisk);
        this.entityRiskList.push(NEW_ENTITY_RISK);
    }

    private updateExistingRiskDetails(): void {
        if (this.editIndex > -1) {
            this.entityRiskList[this.editIndex] = deepCloneObject(this.entityRiskModalDetails.entityRisk);
        }
    }

    private updateEntityRisk(): void {
        this.$subscriptions.push(this._entityRiskSectionService.updateEntityRisk(this.getEntityRO(), this.getProxyController()).subscribe((data: any) => {
            this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Risk updated successfully.');
            this.updateExistingRiskDetails()
            this.clearRiskDetails();
        }))
    }

    private getProxyController(): EntityRiskProxyController {
        switch (this.riskCategoryCode) {
            case 'OR': return '/organization';
            case 'CO': return '/compliance';
            case 'SP': return '/sponsor';
            case 'EN': return '';
            default: return;
        }
    }

    private getEntityRO(): EntityRiskRO {
        return {
            description: this.entityRiskModalDetails.entityRisk.description,
            entityId: this.entityRiskModalDetails.entityRisk.entityId,
            riskTypeCode: this.entityRiskModalDetails.entityRisk.riskTypeCode,
            riskLevelCode: this.entityRiskModalDetails.entityRisk.riskLevelCode,
            entityRiskId: this.entityRiskModalDetails.entityRisk.entityRiskId
        }
    }

    private entityMandatoryValidation(): boolean {
        this.mandatoryList.clear();
        if (!this.entityRiskModalDetails.entityRisk.riskLevelCode) {
            this.mandatoryList.set('riskLevel', 'Please select risk level.');
        }
        if (!this.entityRiskModalDetails.entityRisk.riskTypeCode) {
            this.mandatoryList.set('riskType', 'Please select risk type.');
        }
        if (!this.entityRiskModalDetails.entityRisk.description) {
            this.mandatoryList.set('riskDescription', 'Please enter risk description.');
        }
        return this.mandatoryList.size === 0;
    }

    private setSponsorRiskDetails(risk: EntityRisk): void {
        this.entityRiskModalDetails.entityRisk = deepCloneObject(risk);
        const SELECTED_RISK_TYPE = this.entityRiskTypeList.find((_risk: any) => risk?.riskTypeCode === _risk.riskTypeCode);
        const SELECTED_RISK_LEVEL = this.entityRiskTypeList.find((_risk: any) => risk?.riskLevelCode === _risk.riskLevelCode);
        this.entityRiskModalDetails.selectedRiskTypeLookUpList = [deepCloneObject(SELECTED_RISK_TYPE)];
        this.entityRiskModalDetails.selectedRiskLevelLookUpList = [deepCloneObject(SELECTED_RISK_LEVEL)];
    }

    onRiskTypeSelected(event: any[] | null): void {
        this.entityRiskModalDetails.entityRisk.riskType = event ? event[0] : null;
        this.entityRiskModalDetails.entityRisk.riskTypeCode = event ? event[0]?.riskTypeCode : null;
    }

    onRiskLevelSelected(event: any[] | null): void {
        this.entityRiskModalDetails.entityRisk.riskLevel = event ? event[0] : null;
        this.entityRiskModalDetails.entityRisk.riskLevelCode = event ? event[0]?.riskLevelCode : null;
    }

    riskModalActions(modalAction: ModalActionEvent): void {
        switch (modalAction.action) {
            case 'CLOSE_BTN':
            case 'SECONDARY_BTN':
                return this.clearRiskDetails();
            case 'PRIMARY_BTN':
                return this.addOrUpdateRisk();
            default: break;
        }
    }

    openAddEntityRiskModal(isEditRisk: boolean): void {
        this.isOpenRiskModal = true;
        this.isEditRisk = isEditRisk
        this.entityRiskModalConfig.namings.primaryBtnName = isEditRisk ? 'Update Risk' : 'Add Risk';
        setTimeout(() => {
            openCommonModal(this.ENTITY_RISK_ADD_UPDATE_MODAL_ID);
        }, 100);
    }

    editEntityRisk(risk: EntityRisk, editIndex: number): void {
        this.editIndex = editIndex;
        this.setSponsorRiskDetails(risk);
        this.openAddEntityRiskModal(true);
    }

}
