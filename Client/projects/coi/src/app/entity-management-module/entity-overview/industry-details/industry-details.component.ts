import {Component, OnDestroy, OnInit} from '@angular/core';
import {
    deepCloneObject,
    hideModal,
    isEmptyObject,
    openModal
} from 'projects/fibi/src/app/common/utilities/custom-utilities';
import {IndustryDetails} from '../../shared/entity-interface';
import {forkJoin, Subscription} from 'rxjs';
import {EntityOverviewService} from '../entity-overview.service';
import {EntityDataStoreService} from '../../entity-data-store.service';
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
    industryCategoryTypeOptions = 'INDUSTRY_CATEGORY_TYPE#INDUSTRY_CATEGORY_TYPE_CODE#false#true';
    industryCategoryDescriptionOptions = 'EMPTY#EMPTY#true#true';
    mandatoryList = new Map();
    $subscriptions: Subscription[] = [];
    industryCategoryTypeCode: any;
    entityIndustryCategories = [];
    industryCategoryDescriptionsList: any = [];
    entityId: any;
    entityIndustryClassifications: any[] = [];
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
    addedCategoryIds: Set<number> = new Set();
    CONFIRMATION_MODAL_ID = 'industry-delete-confirm-modal';
    modalConfig = new COIModalConfig(this.CONFIRMATION_MODAL_ID, 'Delete', 'Cancel');

    constructor(private _entityOverviewService: EntityOverviewService,
                private _dataStoreService: EntityDataStoreService,
                private _commonService: CommonService) {}

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
        this.checkUserHasRight();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    clearIndustryDetails() {
        hideModal('addIndustryDetails');
        setTimeout(() => {
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
            this.isSaving = false;
        }, 200);
    }

    addIndustry() {
        if (!this.isSaving && this.validateEntityindustry()) {
            this.isEditIndex != null ? this.updateIndustryDetails() : this.saveIndustryDetails();
        }
    }

    onIndustryCategoryTypeSelect(event) {
        this.entityIndustryCategories = [];
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
            const data: any = await this._entityOverviewService.fetchIndustryCategoryCode(this.industryCategoryTypeCode);
            if (data) {
                this.industryCategoryDescriptionsList = this.isEditIndex == null ?
                    data.filter(item => !this.addedCategoryIds.has(item.industryCategoryId)) : data;
                this.industryCategoryDescriptionsList = this.industryCategoryDescriptionsList.map(item =>
                    ({code: item.industryCategoryId, ...item}));
            }
        } catch (err) {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
        }
    }


    validateEntityindustry(): boolean {
        this.mandatoryList.clear();
        if (!this.industryCategoryTypeCode || !this.industryCategoryTypeCode.length) {
            this.mandatoryList.set('industryCategoryTypeCode', 'Please select industry category type.');
        } else if (this.isEditIndex === null && this.getHasDuplicateTypeCode()) {
            this.mandatoryList.set('industryCategoryTypeCode', 'The industry category type has already been added. Please edit the existing type to make any changes.');
        }
        if (!this.entityIndustryCategories || !this.entityIndustryCategories.length) {
            this.mandatoryList.set('industryCategroyDescription', 'Please select industry category description.');
        }
        return this.mandatoryList.size === 0;
    }

    private getHasDuplicateTypeCode(): boolean {
        const INDUSTRY_TYPE_CODE_GROUP = this.groupBy(this.entityIndustryClassifications, 'industryCategoryCode', 'industryCategoryType', 'industryCategoryTypeCode');
        return this.industryCategoryTypeCode && INDUSTRY_TYPE_CODE_GROUP?.hasOwnProperty(this.industryCategoryTypeCode);
    }

    onIndustryCategoryDescriptionSelect(event) {
        if (event && event.length) {
            this.entityIndustryCategories = event;
            const fullDataCatIds = this.entityIndustryClassifications.map(it => it.industryCategoryId);
            this.addedEntityIndustryCatIds = this.entityIndustryCategories?.filter((item: any) => !fullDataCatIds.includes(item.industryCategoryId));
            const newAddedCatIds = this.entityIndustryCategories.map(it => it.industryCategoryId);
            this.removedEntityIndustryClassIds = this.selectedIndustry?.value?.filter((item: any) => !newAddedCatIds.includes(item.industryCategoryId));
        } else {
            this.entityIndustryCategories = [];
        }
    }

    saveIndustryDetails() {
        this.setReqObj();
        this.isSaving = true;
        this.$subscriptions.push(this._entityOverviewService.saveIndustryDetails(this.industryDetails).subscribe((data: any) => {
            this._dataStoreService.enableModificationHistoryTracking();
            this._dataStoreService.updateStore(['entityIndustryClassifications'], { 'entityIndustryClassifications':  data });
            this.isSaving = false;
            this.clearIndustryDetails();
        }, err => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
            this.isSaving = false;
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
        this.entityIndustryClassifications = entityData.entityIndustryClassifications || [];
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
        return jsonData?.reduce((relationsTypeGroup, item) => {
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
                code: catCode.industryCategoryId,
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

    deleteIndustryClass() {
        if (!this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(forkJoin(this.deleteClassList.map(catCode => this._entityOverviewService
                .deleteIndustryDetailsByClassId(catCode))).subscribe((res) => {
                delete this.entityIndustryClassificationsGrouping[this.selectedIndustry.key];
                this.entityIndustryClassifications = this.entityIndustryClassifications
                    .filter(items => !this.deleteClassList.includes(items.entityIndustryClassId));
                this._dataStoreService.enableModificationHistoryTracking();
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
        this.isSaving = true;
        this.$subscriptions.push(this._entityOverviewService.updateIndustryDetails(this.generateUpdateObj()).subscribe((res: any) => {
            this.entityIndustryClassifications = res;
            this._dataStoreService.enableModificationHistoryTracking();
            this.updateDataStore();
            this.clearIndustryDetails();
            this.isSaving = false;
        }, err => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
            this.isSaving = false;
        }));
    }

    generateUpdateObj() {
        return {
            entityId: this.entityId,
            removedEntityIndustryClassIds: this.removedEntityIndustryClassIds.map(item => item.entityIndustryClassId),
            addedEntityIndustryCatIds: this.addedEntityIndustryCatIds.map(item => item.industryCategoryId),
            primaryCatId: this.getPrimaryCatId(),
            updatePrimaryCatId: this.industryDetails.updatePrimaryCatId
        };
    }

    getPrimaryCatId() {
        return this.industryDetails.primaryCatId ? this.industryDetails.primaryCatId : null;
    }

    updateDataStore() {
        this._dataStoreService.updateStore(['entityIndustryClassifications'],
            { 'entityIndustryClassifications':  this.entityIndustryClassifications });
    }

    clearPrimaryFlag() {
        this.industryDetails.primaryCatId = '';
        this.industryDetails.updatePrimaryCatId = true;
    }

    setPrimaryCatId(industryCategoryId) {
        this.industryDetails.primaryCatId = industryCategoryId;
        this.industryDetails.updatePrimaryCatId = true;
    }

    checkUserHasRight(): void {
        const hasRight = this._commonService.getAvailableRight(['MANAGE_ENTITY'], 'SOME');
        if (!hasRight) {
            this.isEditMode = false;
        }
    }

    addIndustryDetails(event) {
        if (event) {
            openModal('addIndustryDetails', {
                backdrop: 'static',
                keyboard: true,
                focus: false
              });
        }
    }
}
