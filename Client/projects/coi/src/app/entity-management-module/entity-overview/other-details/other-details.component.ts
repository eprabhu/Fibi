import { Component } from '@angular/core';
import { EntityDetails, OtherDetails, showEntityToast } from '../../shared/entity-interface';
import { getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../../common/utilities/date-utilities';
import { CommonService } from '../../../common/services/common.service';
import { DATE_PLACEHOLDER, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { interval, Subject, Subscription } from 'rxjs';
import { debounce } from 'rxjs/operators';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { EntityOverviewService } from '../entity-overview.service';
import { AutoSaveService } from '../../../common/services/auto-save.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { EntityManagementService } from '../../entity-management.service';
import { COIModalConfig, ModalActionEvent } from '../../../shared-components/coi-modal/coi-modal.interface';
import { closeCommonModal, openCommonModal } from '../../../common/utilities/custom-utilities';
import { Router } from '@angular/router';

@Component({
  selector: 'app-other-details',
  templateUrl: './other-details.component.html',
  styleUrls: ['./other-details.component.scss']
})
export class OtherDetailsComponent {

    entityForeignName: string;
    entityPriorName: string;
    startDate: any;
    incorporationDate:any;
    otherDetailsObj: OtherDetails = new OtherDetails();
    datePlaceHolder = DATE_PLACEHOLDER;
    coiCurrencyOptions = 'EMPTY#EMPTY#false#false';
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
    isSaving = false;
    CONFIRMATIN_MODAL_ID = 'other-details-delete-confirm-modal'
    modalConfig = new COIModalConfig(this.CONFIRMATIN_MODAL_ID, 'Delete', 'Cancel');
    deleteForgeinNameObj = null;
    isEditIndex: null | number = null;

    constructor(public commonService: CommonService, private _entityOverviewService: EntityOverviewService,
        private _router: Router,
        public dataStore: EntityDataStoreService, private _autoSaveService: AutoSaveService, private _entityManagementService: EntityManagementService
    ) {}

    ngOnInit() {
        this.getCurrencyList();
        this.triggerSingleSave();
        this.getDataFromStore();
        this.autoSaveSubscribe();
        this.listenDataChangeFromStore();
    }

    private getDataFromStore() {
        const entityData = this.dataStore.getData();
        if (!entityData || isEmptyObject(entityData)) { return; }
        this.entityDetails = entityData.entityDetails;
        this.priorNames = entityData.priorNames;
        this.foreignNames = entityData.foreignNames;
        if(this.entityDetails.currencyCode) {
            this.selectedLookupList.push({'code': this.entityDetails.currencyCode, 'description': null})
        }
        this.setOtherDetailsObject();
    }

    getCurrencyList() {
        this.$subscriptions.push(this._entityOverviewService.fetchCurrencyDetails().subscribe((data: any) => {
            if(data?.length) {
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

    onCurrencyLookupSelect(event){
        if(event) {
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
        this.commonService.hasChangesAvailable = true;
        if(this.otherDetailsObj[key]) {
            this.otherDetailsObj[key] = this.otherDetailsObj[key]?.trim();
            this.autoSaveRO[key] = this.otherDetailsObj[key];
            this._autoSaveService.commonSaveTrigger$.next(true);
        }
      }

      autoSaveSubscribe() {
        this.$subscriptions.push(this._autoSaveService.autoSaveTrigger$.subscribe(event => this.autoSaveAPI()));
      }

    changeEvent(key) {
        this.isOtherDetailsFormChanged = true;
        this.commonService.hasChangesAvailable = true;
        if(this.otherDetailsObj[key]) {
            this.otherDetailsObj[key] = this.otherDetailsObj[key].trim();
            this.autoSaveRO[key] = this.otherDetailsObj[key];
            this.$debounceEvent.next(true);
        }
    }

    autoSaveAPI() {
        if(this.isOtherDetailsFormChanged) {
            if(!this.autoSaveRO.hasOwnProperty('priorName') && !this.autoSaveRO.hasOwnProperty('foreignName')) {
                this.addOtherDetailsAPI();
            } else if(this.autoSaveRO.hasOwnProperty('priorName')){
                this.updatePriorName();
            } else if(this.autoSaveRO.hasOwnProperty('foreignName')){
                this.updateAlternateName();
            }
        }
    }

    addOtherDetailsAPI() {
        if(Object.keys(this.autoSaveRO).length) {
            this.autoSaveRO.entityId = this.entityDetails.entityId;
            this.commonService.setLoaderRestriction();
            this.$subscriptions.push(this._entityOverviewService.updateOtherDetails(this.autoSaveRO).subscribe((data) => {
                this.updateStoreData(this.autoSaveRO);
                this.autoSaveRO = {};
                this.commonService.hasChangesAvailable = false;
                this.isOtherDetailsFormChanged = false;
                this.navigateToRoute();
                showEntityToast('SUCCESS');
            }, err => {
                console.log(err);
                showEntityToast('ERROR');
            }));
            this.commonService.removeLoaderRestriction();
        }
    }

    navigateToRoute() {
        if(this.commonService.isNavigationStopped) {
            this._router.navigateByUrl(this.commonService.attemptedPath);
        }
    }

    updatePriorName() {
            this.commonService.setLoaderRestriction();
            this.$subscriptions.push(this._entityOverviewService.updatePrioirNameDetails({
                'entityId': this.entityDetails.entityId,
                'priorName': this.otherDetailsObj['priorName']
            }).subscribe((data: any) => {
                this.priorNames.unshift({'priorNames':this.autoSaveRO['priorName'] , 'id': data.id});
                this.dataStore.updateStore(['priorNames'], { 'priorNames':  this.priorNames });
                delete this.autoSaveRO['priorName'];
                delete this.otherDetailsObj['priorName'];
                this.entityPriorName = '';
                this.addOtherDetailsAPI();
                this.commonService.hasChangesAvailable = false;
                this.isOtherDetailsFormChanged = false;
                this.navigateToRoute();
                showEntityToast('SUCCESS');
            }, err => {
                console.log(err);
                showEntityToast('ERROR');
            }));
            this.commonService.removeLoaderRestriction();
    }

    updateAlternateName() {
            this.commonService.setLoaderRestriction();
            this.$subscriptions.push(this._entityOverviewService.updateAlternateNameDetails({
                'entityId': this.entityDetails.entityId,
                'foreignName': this.otherDetailsObj['foreignName']
            }).subscribe((data: any) => {
                this.foreignNames.unshift({'foreignName':this.autoSaveRO['foreignName'] , 'id': data.id});
                this.dataStore.updateStore(['foreignNames'], { 'foreignNames':  this.foreignNames });
                delete this.autoSaveRO['foreignName'];
                delete this.otherDetailsObj['foreignName'];
                this.entityForeignName = '';
                this.addOtherDetailsAPI();
                this.commonService.hasChangesAvailable = false;
                this.isOtherDetailsFormChanged = false;
                this.navigateToRoute();
                showEntityToast('SUCCESS');
            }, err => {
                console.log(err);
                showEntityToast('ERROR');
            }));
            this.commonService.removeLoaderRestriction();
    }

    addEntityPriorName() {
        this.otherDetailsObj.priorName = this.entityPriorName;
        // this.changeEvent('priorName');
        this.immediateAPICall('priorName');
    }

    addEntityAlternatename() {
        this.otherDetailsObj.foreignName = this.entityForeignName;
        // this.changeEvent('foreignName');
        this.immediateAPICall('foreignName');
    }

    deleteEntityForeignName(foreignName, index) {
        this.isEditIndex = index;
        this.deleteForgeinNameObj = foreignName;
        openCommonModal(this.CONFIRMATIN_MODAL_ID);
    }

    onDateSelect(dateType: 'START'| 'INCORPORATION') {
        if(dateType == 'START') {
            this.otherDetailsObj.startDate = parseDateWithoutTimestamp(this.startDate);
            this.changeEvent('startDate');
        }
        if(dateType == 'INCORPORATION') {
            this.otherDetailsObj.incorporationDate = parseDateWithoutTimestamp(this.incorporationDate)
            this.changeEvent('incorporationDate');
        }
    }

    onBusinessTypeSelect(event) {
        if(event) {
            this.otherDetailsObj.businessTypeCode = event[0]?.code;
            this.changeEvent('businessTypeCode');
        } else {
            this.otherDetailsObj.businessTypeCode = null;
        }
    }

    updateStoreData(requestObj) {
        if(!isEmptyObject(requestObj)) {
            Object.keys(requestObj).forEach((ele) =>{
                if(ele != 'priorName' && ele!= 'foreignName') {
                    this.entityDetails[ele] = requestObj[ele];
                } else if(ele == 'priorName') {
                    this.priorNames.push()
                }
            });
            this.dataStore.updateStore(['entityDetails'], { 'entityDetails':  this.entityDetails });
        }
    }

    postConfirmation(modalAction: ModalActionEvent) {
        if(modalAction.action == 'PRIMARY_BTN') {
            this.deleteForgeinName();
        }
        closeCommonModal(this.CONFIRMATIN_MODAL_ID);
    }

    deleteForgeinName() {
        if(!this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(this._entityOverviewService.deleteForeignName(this.deleteForgeinNameObj.id).subscribe((res: any) => {
                this.foreignNames.splice(this.isEditIndex, 1);
                this.deleteForgeinNameObj = null;
                this.isSaving = false;
            }, err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                this.isSaving = false;
            }))
        }
    }

    ngOnDestroy() {
        this.commonService.hasChangesAvailable = false;
        subscriptionHandler(this.$subscriptions);
    }

}