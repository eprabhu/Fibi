import {Component, OnDestroy, OnInit} from '@angular/core';
import {EntityDetails, OtherDetails, showEntityToast} from '../../shared/entity-interface';
import {getDateObjectFromTimeStamp, parseDateWithoutTimestamp} from '../../../common/utilities/date-utilities';
import {CommonService} from '../../../common/services/common.service';
import {DATE_PLACEHOLDER, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS} from '../../../app-constants';
import {interval, Subject, Subscription} from 'rxjs';
import {debounce} from 'rxjs/operators';
import {hideModal, isEmptyObject} from 'projects/fibi/src/app/common/utilities/custom-utilities';
import {EntityDataStoreService} from '../../entity-data-store.service';
import {EntityOverviewService} from '../entity-overview.service';
import {AutoSaveService} from '../../../common/services/auto-save.service';
import {subscriptionHandler} from 'projects/fibi/src/app/common/utilities/subscription-handler';
import {EntityManagementService} from '../../entity-management.service';
import {COIModalConfig, ModalActionEvent} from '../../../shared-components/coi-modal/coi-modal.interface';
import {closeCommonModal, openCommonModal} from '../../../common/utilities/custom-utilities';
import { Router } from '@angular/router';

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
    otherDetailsObj: OtherDetails = new OtherDetails();
    datePlaceHolder = DATE_PLACEHOLDER;
    coiCurrencyOptions = 'EMPTY#EMPTY#false#true';
    businessStatusTypeOptions = 'entity_business_type#BUSINESS_TYPE_CODE#false#false'
    $debounceEvent = new Subject<any>();
    $subscriptions: Subscription[] = [];
    entityDetails: EntityDetails = new EntityDetails();
    priorNames: any = [];
    foreignNames: any = [];
    coiCurrencyList: any = [];
    autoSaveRO: any = {};
    isOtherDetailsFormChanged: boolean;
    selectedLookupList = [];
    selectedBusinessTypeList = [];
    isSaving = false;
    isSavingForForeign = false;
    isSavingForPrior = false;
    CONFIRMATIN_MODAL_ID = 'other-details-delete-confirm-modal'
    modalConfig = new COIModalConfig(this.CONFIRMATIN_MODAL_ID, 'Delete', 'Cancel');
    deleteForgeinNameObj = null;
    isEditIndex: null | number = null;
    isEditMode = false;
    deletePriorNameObj = null;

    constructor(public commonService: CommonService, private _entityOverviewService: EntityOverviewService,
                private _router: Router,public dataStore: EntityDataStoreService, private _autoSaveService: AutoSaveService,
                private _entityManagementService: EntityManagementService
    ) {
    }

    ngOnInit() {
        this.getCurrencyList();
        this.triggerSingleSave();
        this.getDataFromStore();
        this.autoSaveSubscribe();
        this.listenDataChangeFromStore();
        this.checkUserHasRight();
    }

    private getDataFromStore() {
        this.selectedLookupList = [];
        this.selectedBusinessTypeList = [];
        const entityData = this.dataStore.getData();
        if (!entityData || isEmptyObject(entityData)) {
            return;
        }
        this.entityDetails = entityData.entityDetails;
        this.priorNames = entityData.priorNames;
        this.foreignNames = entityData.foreignNames;
        if (this.entityDetails.currencyCode) {
            this.selectedLookupList.push({'code': this.entityDetails.currencyCode, 'description': null});
        }
        if (this.entityDetails.businessTypeCode) {
            this.selectedBusinessTypeList.push({'code': this.entityDetails?.businessTypeCode,
                description: null
            });
        }
        this.setOtherDetailsObject();
        this.isEditMode = this.dataStore.getEditMode();
    }

    getCurrencyList() {
        this.$subscriptions.push(this._entityOverviewService.fetchCurrencyDetails().subscribe((data: any) => {
            if (data?.length) {
                this.coiCurrencyList = data.map(ele => {
                    const lookUp: any = {};
                    lookUp.code = ele.currencyCode;
                    lookUp.description = ele.currency + '(' + ele.currencyCode + ')';
                    return lookUp;
                });
            }
        }))
    }

    setOtherDetailsObject() {
        this.startDate = getDateObjectFromTimeStamp(this.entityDetails?.startDate);
        this.incorporationDate = getDateObjectFromTimeStamp(this.entityDetails?.incorporationDate);
        this.otherDetailsObj.startDate = this.entityDetails?.startDate;
        this.otherDetailsObj.incorporationDate = this.entityDetails?.incorporationDate;
        this.otherDetailsObj.activityText = this.entityDetails?.activityText;
        this.otherDetailsObj.congressionalDistrict = this.entityDetails?.congressionalDistrict;
        this.otherDetailsObj.incorporatedIn = this.entityDetails?.incorporatedIn;
        this.otherDetailsObj.federalEmployerId = this.entityDetails?.federalEmployerId;
        this.otherDetailsObj.numberOfEmployees = this.entityDetails?.numberOfEmployees;
        this.otherDetailsObj.shortName = this.entityDetails?.shortName;
        this.otherDetailsObj.currencyCode = this.entityDetails?.currencyCode;
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this.dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    onCurrencyLookupSelect(event) {
        if (event) {
            this.otherDetailsObj.currencyCode = event[0]?.code;
            this.changeEvent('currencyCode');
        } else {
            this.otherDetailsObj.currencyCode = null;
        }
    }

    triggerSingleSave() {
        this.$subscriptions.push(this.$debounceEvent.pipe(debounce(() => interval(2000))).subscribe((data: any) => {
                if (data) {
                    this._autoSaveService.commonSaveTrigger$.next(true);
                    // this.autoSaveAPI();
                }
            }
        ));
    }

    immediateAPICall(key) {
        this.isOtherDetailsFormChanged = true;
        if(this.otherDetailsObj[key]) {
            this.otherDetailsObj[key] = this.otherDetailsObj[key]?.trim();
            this.autoSaveRO[key] = this.otherDetailsObj[key];
            this._autoSaveService.autoSaveTrigger$.next();
        }
    }

    autoSaveSubscribe() {
        this.$subscriptions.push(this._autoSaveService.autoSaveTrigger$.subscribe(event => this.autoSaveAPI()));
    }

    changeEvent(key) {
        this.commonService.setChangesAvailable(true);
        this.isOtherDetailsFormChanged = true;
        if(this.otherDetailsObj[key]) {
            this.otherDetailsObj[key] = this.otherDetailsObj[key].trim();
            this.autoSaveRO[key] = this.otherDetailsObj[key];
            this.$debounceEvent.next(true);
        }
    }

    autoSaveAPI() {
        if (this.isOtherDetailsFormChanged) {
            if (!this.autoSaveRO.hasOwnProperty('priorName') && !this.autoSaveRO.hasOwnProperty('foreignName')) {
                this.addOtherDetailsAPI();
            } else if (this.autoSaveRO.hasOwnProperty('priorName')) {
                this.updatePriorName();
            } else if (this.autoSaveRO.hasOwnProperty('foreignName')) {
                this.updateAlternateName();
            }
        }
    }

    addOtherDetailsAPI() {
        if (!this.isSaving && Object.keys(this.autoSaveRO).length) {
            this.isSaving = true;
            this.autoSaveRO.entityId = this.entityDetails.entityId;
            this.commonService.setLoaderRestriction();
            this.$subscriptions.push(this._entityOverviewService.updateOtherDetails(this.autoSaveRO).subscribe((data) => {
                this.updateStoreData(this.autoSaveRO);
                this.autoSaveRO = {};
                this.isOtherDetailsFormChanged = false;
                this.isSaving = false;
                showEntityToast('SUCCESS');
                this.commonService.setChangesAvailable(false);
            }, err => {
                this.isSaving = false;
                console.log(err);
                showEntityToast('ERROR');
            }));
            this.commonService.removeLoaderRestriction();
        }
    }

    updatePriorName() {
        if (!this.isSavingForPrior) {
            this.isSavingForPrior = true;
            this.$subscriptions.push(this._entityOverviewService.updatePrioirNameDetails({
                'entityId': this.entityDetails.entityId,
                'priorName': this.otherDetailsObj['priorName']
            }).subscribe((data: any) => {
                this.priorNames.unshift({'priorNames': this.autoSaveRO['priorName'], 'id': data.id});
                this.updateDataStore('priorNames');
                delete this.autoSaveRO['priorName'];
                delete this.otherDetailsObj['priorName'];
                this.entityPriorName = '';
                this.addOtherDetailsAPI();
                this.isOtherDetailsFormChanged = false;
                this.isSavingForPrior = false;
                showEntityToast('SUCCESS');
                this.commonService.setChangesAvailable(false);
            }, err => {
                console.log(err);
                this.isSavingForPrior = false;
                showEntityToast('ERROR');
            }));
        }
    }

    updateAlternateName() {
        if(!this.isSavingForForeign) {
            this.isSavingForForeign = true;
            this.$subscriptions.push(this._entityOverviewService.updateAlternateNameDetails({
                'entityId': this.entityDetails.entityId,
                'foreignName': this.otherDetailsObj['foreignName']
            }).subscribe((data: any) => {
                this.foreignNames.unshift({'foreignName': this.autoSaveRO['foreignName'], 'id': data.id});
                this.updateDataStore('foreignNames');
                delete this.autoSaveRO['foreignName'];
                delete this.otherDetailsObj['foreignName'];
                this.entityForeignName = '';
                this.addOtherDetailsAPI();
                this.isOtherDetailsFormChanged = false;
                this.isSavingForForeign = false;
                showEntityToast('SUCCESS');
                this.commonService.setChangesAvailable(false);
            }, err => {
                console.log(err);
                this.isSavingForForeign = false;
                showEntityToast('ERROR');
            }));
        }
    }

    addEntityPriorName() {
        if (!this.entityPriorName.trim()) { return; }
        this.otherDetailsObj.priorName = this.entityPriorName.trim();
        // this.changeEvent('priorName');
        this.immediateAPICall('priorName');
    }

    addEntityAlternatename() {
        if (!this.entityForeignName.trim()) { return; }
        this.otherDetailsObj.foreignName = this.entityForeignName.trim();
        // this.changeEvent('foreignName');
        this.immediateAPICall('foreignName');
    }

    deleteEntityForeignName(foreignName, index) {
        this.isEditIndex = index;
        this.deleteForgeinNameObj = foreignName;
        openCommonModal(this.CONFIRMATIN_MODAL_ID);
    }

    onDateSelect(dateType: 'START' | 'INCORPORATION') {
        this.commonService.setChangesAvailable(true);
        if (dateType == 'START') {
            this.otherDetailsObj.startDate = parseDateWithoutTimestamp(this.startDate);
            this.changeEvent('startDate');
        }
        if (dateType == 'INCORPORATION') {
            this.otherDetailsObj.incorporationDate = parseDateWithoutTimestamp(this.incorporationDate)
            this.changeEvent('incorporationDate');
        }
    }

    onBusinessTypeSelect(event) {
        if (event) {
            this.otherDetailsObj.businessTypeCode = event[0]?.code;
            this.changeEvent('businessTypeCode');
        } else {
            this.otherDetailsObj.businessTypeCode = null;
        }
    }

    updateStoreData(requestObj) {
        if (!isEmptyObject(requestObj)) {
            Object.keys(requestObj).forEach((ele) => {
                if (ele != 'priorName' && ele != 'foreignName') {
                    this.entityDetails[ele] = requestObj[ele];
                } else if (ele == 'priorName') {
                    this.priorNames.push();
                }
            });
            this.dataStore.updateStore(['entityDetails'], {'entityDetails': this.entityDetails});
        }
    }

    postConfirmation(modalAction: ModalActionEvent) {
        if (modalAction.action == 'PRIMARY_BTN') {
            if (this.deletePriorNameObj) {
                this.deletePriorName();
            } else {
                this.deleteForeignName();
            }
        }
        closeCommonModal(this.CONFIRMATIN_MODAL_ID);
    }

    deleteForeignName() {
        if (!this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(this._entityOverviewService.deleteForeignName(this.deleteForgeinNameObj.id).subscribe((res: any) => {
                this.foreignNames.splice(this.isEditIndex, 1);
                this.deleteForgeinNameObj = null;
                this.isEditIndex = null;
                this.isSaving = false;
                this.updateDataStore('foreignNames');
            }, err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                this.isSaving = false;
            }));
        }
    }

    confirmDeleteName(priorName, index: number) {
        this.deletePriorNameObj = priorName;
        this.isEditIndex = index;
        hideModal('entityPriorNameVersionModal');
        openCommonModal(this.CONFIRMATIN_MODAL_ID);
    }

    deletePriorName() {
        if (!this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(this._entityOverviewService.deletePriorName(this.deletePriorNameObj.id).subscribe((res: any) => {
                this.priorNames.splice(this.isEditIndex, 1);
                this.isEditIndex = null;
                this.deletePriorNameObj = null;
                this.isSaving = false;
                this.updateDataStore('priorNames');
            }, err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                this.isSaving = false;
            }));
        }
    }

    updateDataStore(updateKey: 'priorNames' | 'foreignNames') {
        this.dataStore.updateStore([updateKey], {[updateKey]: this[updateKey]});
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    checkUserHasRight(): void {
        const hasRight = this.commonService.getAvailableRight(['MANAGE_ENTITY'], 'SOME');
        if (!hasRight) {
            this.isEditMode = false;
        }
    }

}
