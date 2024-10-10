import { Component, OnDestroy, OnInit } from '@angular/core';
import { EntityDetails, EntityTabStatus, LookUpClass, OtherDetailsClass, OtherDetailsUpdate, removeToast, showEntityToast } from '../../shared/entity-interface';
import { getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../../common/utilities/date-utilities';
import { CommonService } from '../../../common/services/common.service';
import { COMMON_ERROR_TOAST_MSG, DATE_PLACEHOLDER, ENTITY_VERIFICATION_STATUS, HTTP_ERROR_STATUS } from '../../../app-constants';
import { forkJoin, interval, Subject, Subscription } from 'rxjs';
import { debounce } from 'rxjs/operators';
import { deepCloneObject, hideModal, isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { EntityOverviewService } from '../entity-overview.service';
import { AutoSaveService } from '../../../common/services/auto-save.service';
import { COIModalConfig, ModalActionEvent } from '../../../shared-components/coi-modal/coi-modal.interface';
import { closeCommonModal, inputRestrictionForNumberField, openCommonModal } from '../../../common/utilities/custom-utilities';
import { AUTO_SAVE_DEBOUNCE_TIME } from '../../shared/entity-constants';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';

@Component({
    selector: 'app-other-details',
    templateUrl: './other-details.component.html',
    styleUrls: ['./other-details.component.scss']
})
export class OtherDetailsComponent implements OnInit, OnDestroy {

    entityForeignName: string;
    entityPriorName: string;
    startDate: any;
    incorporationDate: any;
    datePlaceHolder = DATE_PLACEHOLDER;
    $debounceEvent = new Subject<string>();
    $subscriptions: Subscription[] = [];
    entityDetails: EntityDetails = new EntityDetails();
    priorNames: any = [];
    foreignNames: any = [];
    coiCurrencyList: any = [];
    COI_CURRENCY_OPTIONS = 'EMPTY#EMPTY#false#true';
    BUSINESS_TYPE_OPTIONS = 'entity_business_type#BUSINESS_TYPE_CODE#false#false';
    CONFIRMATIN_MODAL_ID = 'other-details-delete-confirm-modal';
    selectedCurrencyLookupList = [];
    selectedBusinessTypeList = [];
    isSavingForAutoSave = false;
    isSavingForForeign = false;
    isSavingForPrior = false;
    modalConfig = new COIModalConfig(this.CONFIRMATIN_MODAL_ID, 'Delete', 'Cancel');
    deleteForgeinNameObj = null;
    isEditIndex: null | number = null; //updateIndex
    isEditMode = false;
    deletePriorNameObj = null;
    entityTabStatus: EntityTabStatus = new EntityTabStatus();
    otherDetailsObj: OtherDetailsUpdate = new OtherDetailsUpdate();
    changeDetectionObj = new OtherDetailsClass();

    constructor(public commonService: CommonService, private _entityOverviewService: EntityOverviewService,
        public dataStore: EntityDataStoreService, private _autoSaveService: AutoSaveService) { }

    ngOnInit() {
        this.getCurrencyList();
        this.triggerSingleSave();
        this.getDataFromStore();
        this.autoSaveSubscribe();
        this.listenDataChangeFromStore();
        this.updateSelectedLookupList();
        this.setOtherDetailsObject();
    }

    private getCurrencyList(): void {
        this.$subscriptions.push(this._entityOverviewService.fetchCurrencyDetails().subscribe((data: any) => {
            if (data?.length) {
                this.coiCurrencyList = data.map(ele => {
                    const LOOKUP = new LookUpClass();
                    LOOKUP.code = ele.currencyCode;
                    LOOKUP.description = ele.currency + '(' + ele.currencyCode + ')';
                    return LOOKUP;
                });
            }
        }));
    }

    private triggerSingleSave(): void {
        this.$subscriptions.push(this.$debounceEvent.pipe(debounce(() => interval(AUTO_SAVE_DEBOUNCE_TIME))).subscribe((data: any) => {
            if (data) {
                this._autoSaveService.commonSaveTrigger$.next(true);
            }
        }
        ));
    }

    private autoSaveSubscribe(): void {
        this.$subscriptions.push(this._autoSaveService.autoSaveTrigger$.subscribe(event => this.addOtherDetailsAPI()));
    }

    private getDataFromStore(): void {
        const ENTITY_DATA = this.dataStore.getData();
        if (!ENTITY_DATA || isEmptyObject(ENTITY_DATA)) {
            return;
        }
        this.entityDetails = ENTITY_DATA.entityDetails;
        this.priorNames = ENTITY_DATA.priorNames;
        this.foreignNames = ENTITY_DATA.foreignNames;
        this.entityTabStatus = ENTITY_DATA.entityTabStatus;
        this.checkUserHasRight();
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this.dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private updateSelectedLookupList(): void {
        this.selectedCurrencyLookupList = [];
        this.selectedBusinessTypeList = [];
        if (this.entityDetails?.currencyCode) {
            this.selectedCurrencyLookupList.push({ code: this.entityDetails?.currencyCode });
        }
        if (this.entityDetails?.businessTypeCode) {
            this.selectedBusinessTypeList.push({ code: this.entityDetails?.businessTypeCode });
        }
    }

    private setOtherDetailsObject(): void {
        if (!this.entityDetails) {
            return;
        }
        Object.keys(this.entityDetails).forEach(key => {
            this.otherDetailsObj.otherDetailsRequestFields[key] = this.entityDetails[key];
        });
        this.startDate = getDateObjectFromTimeStamp(this.entityDetails.startDate);
        this.incorporationDate = getDateObjectFromTimeStamp(this.entityDetails.incorporationDate);
    }

    private checkUserHasRight(): void {
        this.isEditMode = this.dataStore.getEditMode() && this.commonService.getAvailableRight(['MANAGE_ENTITY'], 'SOME');
    }

    onCurrencyLookupSelect(event: any): void {
        this.otherDetailsObj.otherDetailsRequestFields.currencyCode = event?.[0]?.code || null;
        this.changeEvent('currencyCode');
    }

    changeEvent(key: string): void {
        this.commonService.setChangesAvailable(true);
        this.changeDetectionObj[key] = true;
        this.$debounceEvent.next(key);
    }

    private setupBeforeAPICall(): void {
        removeToast('SUCCESS');
        removeToast('ERROR');
        this.commonService.autoSaveSavingLoader = 'SHOW';
    }

    private getAutoSaveRO(): OtherDetailsUpdate {
        const AUTO_SAVE_PAYLOAD = new OtherDetailsUpdate();
        AUTO_SAVE_PAYLOAD.entityId = this.entityDetails.entityId;
        Object.keys(this.changeDetectionObj).forEach((ele) => {
            if (this.changeDetectionObj[ele]) {
                const VALUE = this.otherDetailsObj.otherDetailsRequestFields[ele];
                AUTO_SAVE_PAYLOAD.otherDetailsRequestFields[ele] = typeof VALUE === 'string' ? (VALUE?.trim() || null) : VALUE;
            }
        });
        return AUTO_SAVE_PAYLOAD;
    }

    private addOtherDetailsAPI(): void {
            const TEMP_AUTO_SAVE_RO: OtherDetailsUpdate = deepCloneObject(this.getAutoSaveRO());
            if (isEmptyObject(TEMP_AUTO_SAVE_RO) || isEmptyObject(TEMP_AUTO_SAVE_RO.otherDetailsRequestFields)) {
                return;
            }
            this.setupBeforeAPICall();
            this.setChangesObject(TEMP_AUTO_SAVE_RO.otherDetailsRequestFields, false);
            this.commonService.setLoaderRestriction();
            this.$subscriptions.push(this._entityOverviewService.updateOtherDetails(TEMP_AUTO_SAVE_RO).subscribe((data) => {
                this.handleAPISuccess(TEMP_AUTO_SAVE_RO);
            }, err => {
                this.setChangesObject(TEMP_AUTO_SAVE_RO.otherDetailsRequestFields, true);
                this.commonService.isShowLoader.next(false);
                this.stopAutoSaveLoader('ERROR');
            }));
            this.commonService.removeLoaderRestriction();
    }

    private handleAPISuccess(autoSaveReqObj: OtherDetailsUpdate): void {
        this.dataStore.enableModificationHistoryTracking();
        this.updateSponsorOrgFeed(this.entityDetails?.entityId, autoSaveReqObj);
        this.updateStoreData(autoSaveReqObj.otherDetailsRequestFields);
        this.commonService.setChangesAvailable(false);
        this.stopAutoSaveLoader('SUCCESS');
    }

    private updateStoreData(otherDetailsRequestFields: OtherDetailsClass): void {
        if (!isEmptyObject(otherDetailsRequestFields)) {
            Object.keys(otherDetailsRequestFields).forEach((ele) => {
                this.entityDetails[ele] = otherDetailsRequestFields[ele];
            });
            this.dataStore.updateStore(['entityDetails'], { 'entityDetails': this.entityDetails });
        }
    }

    private stopAutoSaveLoader(toastType: 'SUCCESS' | 'ERROR'): void {
        setTimeout(() => {
            if (!this.commonService.loaderRestrictedUrls.length) {
                this.commonService.autoSaveSavingLoader = 'HIDE';
                showEntityToast(toastType);
            }
        });
    }

    private setChangesObject(autoSaveReqObj: OtherDetailsClass, isChangesAvailable: boolean): void {
        Object.keys(autoSaveReqObj).forEach((ele) => {
            this.changeDetectionObj[ele] = isChangesAvailable;
        });
    }

    private updateSponsorOrgFeed(entityId: any, reqObj: OtherDetailsUpdate): void {
        const FEED_API_CALLS = this.dataStore.getApiCalls(entityId, reqObj);
        if (FEED_API_CALLS.length && this.entityDetails.entityStatusType.entityStatusTypeCode == ENTITY_VERIFICATION_STATUS.VERIFIED) {
            this.$subscriptions.push(forkJoin(FEED_API_CALLS).subscribe((data: [] = []) => {
                this.dataStore.updateFeedStatus(this.entityTabStatus, 'BOTH');
            }, err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in updating feed status.');
            }
            ));
        }
    }

    checkForValidNumber(event: any): void {
        if (inputRestrictionForNumberField(String.fromCharCode(event.charCode))) {
            event.preventDefault();
        }
    }

    private updatePriorName(): void {
        if (!this.isSavingForPrior) {
            this.isSavingForPrior = true;
            this.setupBeforeAPICall();
            this.commonService.setLoaderRestriction();
            this.$subscriptions.push(this._entityOverviewService.updatePrioirNameDetails({
                'entityId': this.entityDetails?.entityId,
                'priorName': this.entityPriorName
            }).subscribe((data: any) => {
                this.priorNames.unshift({ 'priorNames': this.entityPriorName, 'id': data.id });
                this.updatePriorOrForeignName('priorNames');
            }, err => {
                this.isSavingForPrior = false;
                this.stopAutoSaveLoader('ERROR');
            }));
        }
        this.commonService.removeLoaderRestriction();
    }

    private updateAlternateName(): void {
        if (!this.isSavingForForeign) {
            this.isSavingForForeign = true;
            this.setupBeforeAPICall();
            this.commonService.setLoaderRestriction();
            this.$subscriptions.push(this._entityOverviewService.updateAlternateNameDetails({
                'entityId': this.entityDetails?.entityId,
                'foreignName': this.entityForeignName
            }).subscribe((data: any) => {
                this.foreignNames.unshift({ 'foreignName': this.entityForeignName, 'id': data.id });
                this.updatePriorOrForeignName('foreignNames');
            }, err => {
                this.isSavingForForeign = false;
                this.stopAutoSaveLoader('ERROR');
            }));
            this.commonService.removeLoaderRestriction();
        }
    }

    private updatePriorOrForeignName(type: 'priorNames' | 'foreignNames'): void {
        if (type === 'priorNames') {
            this.entityPriorName = '';
            this.isSavingForPrior = false;
        } else if (type === 'foreignNames') {
            this.entityForeignName = '';
            this.isSavingForForeign = false;
        }
        this.dataStore.enableModificationHistoryTracking();
        this.dataStore.updateStore([type], { [type]: this[type] });
        this.stopAutoSaveLoader('SUCCESS');
        this.commonService.setChangesAvailable(false);
    }

    addEntityName(name: 'FOREIGN' | 'PRIOR'): void {
        if (name === 'PRIOR') {
            this.entityPriorName = this.entityPriorName?.trim();
            if (!this.entityPriorName) { return; }
            this.updatePriorName();
        } else if (name === 'FOREIGN') {
            this.entityForeignName = this.entityForeignName?.trim();
            if (!this.entityForeignName) { return; }
            this.updateAlternateName();
        }
    }

    deleteEntityForeignName(foreignName: string, index: number): void {
        this.isEditIndex = index;
        this.deleteForgeinNameObj = foreignName;
        openCommonModal(this.CONFIRMATIN_MODAL_ID);
    }

    confirmPriorNameDelete(priorName: string, index: number): void {
        this.deletePriorNameObj = priorName;
        this.isEditIndex = index;
        hideModal('entityPriorNameVersionModal');
        openCommonModal(this.CONFIRMATIN_MODAL_ID);
    }

    onDateSelect(dateType: 'incorporationDate' | 'startDate'): void {
        this.commonService.setChangesAvailable(true);
        const SELECTED_DATE = dateType === 'incorporationDate' ? this.incorporationDate : this.startDate;
        this.otherDetailsObj.otherDetailsRequestFields[dateType] = parseDateWithoutTimestamp(SELECTED_DATE);
        this.changeEvent(dateType);
    }

    onBusinessTypeSelect(event: any): void {
        this.otherDetailsObj.otherDetailsRequestFields.businessTypeCode = event?.[0]?.code || null;
        this.changeEvent('businessTypeCode');
    }

    postConfirmation(modalAction: ModalActionEvent): void {
        if (modalAction.action == 'PRIMARY_BTN') {
            if (this.deletePriorNameObj) {
                this.deletePriorName();
            } else {
                this.deleteForeignName();
            }
        }
    }

    private deleteForeignName(): void {
        this.setupBeforeAPICall();
        this.commonService.setLoaderRestriction();
        this.$subscriptions.push(this._entityOverviewService.deleteForeignName(this.deleteForgeinNameObj.id).subscribe((res: any) => {
            this.foreignNames.splice(this.isEditIndex, 1);
            this.handleApiResponse('foreignNames', 'SUCCESS');
        }, err => {
            this.handleApiResponse('foreignNames', 'ERROR');
        }));
        this.commonService.removeLoaderRestriction();
    }

    private deletePriorName(): void {
        this.setupBeforeAPICall();
        this.commonService.setLoaderRestriction();
        this.$subscriptions.push(this._entityOverviewService.deletePriorName(this.deletePriorNameObj.id).subscribe((res: any) => {
            this.priorNames.splice(this.isEditIndex, 1);
            this.handleApiResponse('priorNames', 'SUCCESS');
        }, err => {
            this.handleApiResponse('priorNames', 'ERROR');
        }));
        this.commonService.removeLoaderRestriction();
    }

    private handleApiResponse(type: 'priorNames' | 'foreignNames', status: 'SUCCESS' | 'ERROR'): void {
        if (status === 'SUCCESS') {
            this.stopAutoSaveLoader('SUCCESS');
            this.dataStore.updateStore([type], { [type]: this[type] });
            closeCommonModal(this.CONFIRMATIN_MODAL_ID);
            type === 'foreignNames' ? this.deleteForgeinNameObj = null : this.deletePriorNameObj = null;
            this.isEditIndex = null;
        } else {
            setTimeout(() => {
                if (!this.commonService.loaderRestrictedUrls.length) {
                    this.commonService.autoSaveSavingLoader = 'HIDE';
                }
            });
            this.commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
        }
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

}
