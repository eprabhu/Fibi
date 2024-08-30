import {Component, OnDestroy, OnInit} from '@angular/core';
import { deepCloneObject, hideModal, isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { IndustryDetails } from '../../shared/entity-interface';
import {forkJoin, Subscription} from 'rxjs';
import { EntityOverviewService } from '../entity-overview.service';
import { EntityDataStoreService } from '../../entity-data-store.service';
import {COIModalConfig, ModalActionEvent} from '../../../shared-components/coi-modal/coi-modal.interface';
import {closeCommonModal, openCommonModal} from '../../../common/utilities/custom-utilities';
import {HTTP_ERROR_STATUS} from '../../../app-constants';
import {CommonService} from '../../../common/services/common.service';
import {subscriptionHandler} from '../../../../../../fibi/src/app/common/utilities/subscription-handler';

@Component({
  selector: 'app-industry-details',
  templateUrl: './industry-details.component.html',
  styleUrls: ['./industry-details.component.scss']
})
export class IndustryDetailsComponent implements OnInit, OnDestroy {

    industryDetails: IndustryDetails = new IndustryDetails();
    industryCategoryTypeOptions = 'INDUSTRY_CATEGORY_TYPE#INDUSTRY_CATEGORY_TYPE_CODE#false#false';
    industryCategoryDescriptionOptions = 'EMPTY#EMPTY#true#true';
    mandatoryList = new Map();
    $subscriptions: Subscription[] = [];
    industryCategoryTypeCode: any;
    entityIndustryCategories = [];
    industryCategoryDescriptionsList: any = [];
    entityId: any;
    entityIndustryClassifications: any = [];
    entityIndustryClassificationsGrouping: any = {};
    categoryDescriptionList = [];
    categoryTypeList = [];
    deleteCatCodeList = [];
    deleteClassList = [];
    addedEntityIndustryCatIds = [];
    removedEntityIndustryClassIds = [];
    isEditIndex = null;
    isEditMode = false;
    selectedIndustry = null;
    isSaving = false;
    CONFIRMATION_MODAL_ID = 'industry-delete-confirm-modal';
    modalConfig = new COIModalConfig(this.CONFIRMATION_MODAL_ID, 'Delete', 'Cancel');

    addIndustryDetails(event) {
        if (event) {
            openModal('addIndustryDetails');
        }
    }

    constructor(private _entityOverviewService: EntityOverviewService,
                private _dataStoreService: EntityDataStoreService,
                private _commonService: CommonService) {}

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    clearIndustryDetails() {
        this.mandatoryList.clear();
        this.industryDetails = new IndustryDetails();
        this.categoryTypeList = [];
        this.categoryDescriptionList = [];
        this.industryCategoryTypeCode = null;
        this.entityIndustryCategories = [];
        this.deleteClassList  = [];
        this.deleteCatCodeList = [];
        this.addedEntityIndustryCatIds = [];
        this.removedEntityIndustryClassIds = [];
        this.isEditIndex = null;
        this.selectedIndustry = null;
        hideModal('addIndustryDetails');
    }

    addIndustry() {
        this.entityMandatoryValidation();
        if (!this.mandatoryList.size) {
            this.isEditIndex != null ? this.updateIndustryDetails() : this.saveIndustryDetails();
        }
    }

    onIndustryCategoryTypeSelect(event) {
        this.industryCategoryDescriptionsList = [];
        if (event && event.length) {
            this.industryCategoryTypeCode = event[0].code;
            this.fetchIndustryDescription();
        } else {
            this.industryCategoryTypeCode = null;
        }
    }

    async fetchIndustryDescription() {
        try {
            const data = await this._entityOverviewService.fetchIndustryCategoryCode(this.industryCategoryTypeCode);
            if (data) {
                this.industryCategoryDescriptionsList = deepCloneObject(data);
            }
        } catch (err) {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
        }
    }


    entityMandatoryValidation(): void {
        this.mandatoryList.clear();
        if (!this.industryCategoryTypeCode || !this.industryCategoryTypeCode.length) {
            this.mandatoryList.set('industryCategoryTypeCode', 'Please select industry category type.');
        }
        if (!this.entityIndustryCategories || !this.entityIndustryCategories.length) {
            this.mandatoryList.set('industryCategroyDescription', 'Please select industry category description.');
        }
    }

    onIndustryCategoryDescriptionSelect(event) {
        if (event && event.length) {
            this.entityIndustryCategories = event;
            const fullDataCatIds = this.entityIndustryClassifications.map(it => it.industryCategoryId);
            this.addedEntityIndustryCatIds = this.entityIndustryCategories
                .filter(item => !fullDataCatIds.includes(item.industryCategoryId));
            const newAddedCatIds = this.entityIndustryCategories.map(it => it.industryCategoryId);
            this.removedEntityIndustryClassIds = this.selectedIndustry.value
                .filter(item => !newAddedCatIds.includes(item.industryCategoryId));
        } else {
            this.entityIndustryCategories = [];
        }
    }

    saveIndustryDetails() {
        this.setReqObj();
        this.$subscriptions.push(this._entityOverviewService.saveIndustryDetails(this.industryDetails).subscribe((data: any) => {
            this._dataStoreService.updateStore(['entityIndustryClassifications'], { 'entityIndustryClassifications':  data });
            this.clearIndustryDetails();
        }));
    }

    setReqObj() {
        this.industryDetails.entityIndustryCatIds = [];
        this.industryDetails.entityId = this.entityId;
        this.entityIndustryCategories.forEach((ele) => {
            this.industryDetails.entityIndustryCatIds.push(ele.industryCategoryId);
        });
    }

    private getDataFromStore() {
        const entityData = this._dataStoreService.getData();
        if (isEmptyObject(entityData)) { return; }
        this.entityId = entityData?.entityDetails?.entityId;
        this.entityIndustryClassifications = entityData.entityIndustryClassifications;
        if (this.entityIndustryClassifications.length) {
            this.entityIndustryClassificationsGrouping = this.groupBy(this.entityIndustryClassifications, 'industryCategoryCode', 'industryCategoryType', 'description');
        }
        this.isEditMode = this._dataStoreService.getEditMode();
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    groupBy(jsonData, key, innerKey, secondInnerKey) {
        return jsonData.reduce((relationsTypeGroup, item) => {
            (relationsTypeGroup[item[key][innerKey][secondInnerKey]] = relationsTypeGroup[item[key][innerKey][secondInnerKey]] || []).push(item);
            return relationsTypeGroup;
        }, {});
    }

    async editIndustry(industry, index: number) {
        this.selectedIndustry = deepCloneObject(industry);
        this.isEditIndex = index;
        this.categoryTypeList = this.getCategoryType(industry);
        this.industryCategoryTypeCode = this.categoryTypeList[0].code;
        await this.fetchIndustryDescription();
        this.categoryDescriptionList = this.getCategoryDescription(industry);
        this.entityIndustryCategories = this.getMappedCategory(industry);
        openModal('addIndustryDetails');
    }

    getCategoryType(industry) {
        return [{code: industry?.value[0]?.industryCategoryCode?.industryCategoryTypeCode}];
    }

    getCategoryDescription(industry) {
        if (!industry || !industry.value) { return []; }
        return industry?.value.map((catCode) => {
            if (catCode.isPrimary) {
                this.industryDetails.primaryCatId = catCode.industryCategoryId;
            }
            return {
                entityIndustryClassId: catCode.entityIndustryClassId,
                industryCategoryId: catCode.industryCategoryId,
            description: catCode.industryCategoryCode.description,
            isPrimary: catCode.isPrimary};
        });
    }

    getMappedCategory(industry) {
        if (!industry || !industry.value) { return []; }
        return industry?.value.map((indClass) => ({...indClass.industryCategoryCode, entityIndustryClassId: indClass.entityIndustryClassId }));
    }

    confirmDeleteIndustry(industry, index) {
        this.selectedIndustry = industry;
        this.deleteCatCodeList = industry.value.map((ind) => Number(ind.industryCategoryCode.industryCategoryCode));
        this.deleteClassList = industry.value.map((ind) => ind.entityIndustryClassId);
        this.isEditIndex = index;
        openCommonModal(this.CONFIRMATION_MODAL_ID);
    }

    postConfirmation(modalAction: ModalActionEvent) {
        if (modalAction.action == 'PRIMARY_BTN') {
            // this.deleteIndustryCode();
            this.deleteIndustryClass();
        }
        closeCommonModal(this.CONFIRMATION_MODAL_ID);
    }

    deleteIndustryCode() {
        if (!this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(forkJoin(this.deleteCatCodeList.map(catCode => this._entityOverviewService
                .deleteIndustryDetailsByCatCode(catCode))).subscribe((res) => {

                this.updateDataStore();
                this.clearIndustryDetails();
                this.isSaving = false;
            }, err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                this.isSaving = false;
            }));
        }
    }

    deleteIndustryClass() {
        if (!this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(forkJoin(this.deleteClassList.map(catCode => this._entityOverviewService
                .deleteIndustryDetailsByClassId(catCode))).subscribe((res) => {
                delete this.entityIndustryClassificationsGrouping[this.selectedIndustry.key];
                this.entityIndustryClassifications = this.entityIndustryClassifications
                    .filter(items => !this.deleteClassList.includes(items.entityIndustryClassId));
                this.updateDataStore();
                this.clearIndustryDetails();
                this.isSaving = false;
            }, err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                this.isSaving = false;
            }));
        }
    }

    updateIndustryDetails() {
        if (!this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(this._entityOverviewService.updateIndustryDetails(this.generateUpdateObj()).subscribe((res) => {
                this.entityIndustryClassifications = res;
                this.updateDataStore();
                this.clearIndustryDetails();
                this.isSaving = false;
            }, err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                this.isSaving = false;
            }));
        }
    }

    generateUpdateObj() {
        return {
            entityId: this.entityId,
            removedEntityIndustryClassIds: this.removedEntityIndustryClassIds.map(item => item.entityIndustryClassId),
            addedEntityIndustryCatIds: this.addedEntityIndustryCatIds.map(item => item.industryCategoryId),
            primaryCatId: this.getPrimaryCatId()
        };
    }

    getPrimaryCatId() {
        return this.industryDetails.primaryCatId ? this.industryDetails.primaryCatId : null;
    }

    updateDataStore() {
        this._dataStoreService.updateStore(['entityIndustryClassifications'],
            { 'entityIndustryClassifications':  this.entityIndustryClassifications });
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }
}
