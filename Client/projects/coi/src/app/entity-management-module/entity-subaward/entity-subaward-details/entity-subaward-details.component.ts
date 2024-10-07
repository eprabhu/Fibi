import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { DATE_PLACEHOLDER, ENTITY_VERIFICATION_STATUS } from '../../../app-constants';
import { CommonService } from '../../../common/services/common.service';
import { getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../../common/utilities/date-utilities';
import { AutoSaveService } from '../../../common/services/auto-save.service';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { canUpdateOrgFeed } from '../../entity-management.service';
import { interval, Subject, Subscription } from 'rxjs';
import { debounce } from 'rxjs/operators';
import { EntireEntityDetails, EntityDetails, EntityOrganizationType, EntityTabStatus, removeToast, showEntityToast, SubAwardOrganizationDetails, SubawardOrgFields, SubAwardOrgUpdateClass } from '../../shared/entity-interface';
import { deepCloneObject, isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntitySubAwardService, isOrganizationConditionSatisfied } from '../entity-subaward.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { AUTO_SAVE_DEBOUNCE_TIME } from '../../shared/entity-constants';

@Component({
    selector: 'app-entity-subaward-details',
    templateUrl: './entity-subaward-details.component.html',
    styleUrls: ['./entity-subaward-details.component.scss']
})
export class EntitySubawardDetailsComponent implements OnInit, OnDestroy {

    @Input() sectionName: any;
    @Input() sectionId: any;
    ORGANIZATION_TYPE_OPTIONS = 'entity_organization_type#ORGANIZATION_TYPE_CODE#false#false';
    DATE_PLACEHOLDER = DATE_PLACEHOLDER;
    samExpirationDate: any;
    subAwdRiskAssmtDate: any;
    $debounceEvent = new Subject<any>();
    $subscriptions: Subscription[] = [];
    entityDetails: EntityDetails;
    selectedLookupList: any[] = [];
    isSaving = false;
    isEditMode = false;
    entityTabStatus: EntityTabStatus = new EntityTabStatus();
    subAwardOrg = new SubAwardOrgUpdateClass();
    changeDetectionObj = new SubawardOrgFields();

    constructor(public commonService: CommonService,
        private _dataStoreService: EntityDataStoreService,
        private _autoSaveService: AutoSaveService,
        public entitySubAwardService: EntitySubAwardService
    ) { }

    ngOnInit() {
        this.triggerSingleSave();
        this.listenAutoSave();
        this.getDataFromStore();
        this.getValuesFromService(deepCloneObject(this.entitySubAwardService.entitySubAwardOrganization?.subAwdOrgDetailsResponseDTO));
        this.listenDataChangeFromStore();
    }

    triggerSingleSave(): void {
        this.$subscriptions.push(this.$debounceEvent.pipe(debounce(() => interval(AUTO_SAVE_DEBOUNCE_TIME))).subscribe((data: any) => {
            if (data) {
                this._autoSaveService.commonSaveTrigger$.next(true);
            }
        }
        ));
    }

    listenAutoSave(): void {
        this.$subscriptions.push(this._autoSaveService.autoSaveTrigger$.subscribe((event: any) => {
            this.autoSaveAPI();
        }));
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private getDataFromStore(): void {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        this.entityDetails = ENTITY_DATA.entityDetails;
        this.entityTabStatus = ENTITY_DATA?.entityTabStatus;
        this.checkUserHasRight();
    }

    private checkUserHasRight(): void {
        this.isEditMode = this._dataStoreService.getEditMode() && this.commonService.getAvailableRight(['MANAGE_ENTITY_ORGANIZATION'], 'SOME');
    }

    private getValuesFromService(subAwardOrgService: SubAwardOrganizationDetails): void {
        if (subAwardOrgService?.entityOrganizationType?.organizationTypeCode) {
            this.selectedLookupList.push({ code: subAwardOrgService.entityOrganizationType?.organizationTypeCode });
            this.subAwardOrg.subAwardOrgFields.organizationTypeCode = subAwardOrgService.entityOrganizationType?.organizationTypeCode;
        }
        if (subAwardOrgService?.samExpirationDate) {
            this.samExpirationDate = getDateObjectFromTimeStamp(subAwardOrgService?.samExpirationDate);
            this.subAwardOrg.subAwardOrgFields.samExpirationDate = subAwardOrgService?.samExpirationDate;
        }
        if (subAwardOrgService?.subAwdRiskAssmtDate) {
            this.subAwdRiskAssmtDate = getDateObjectFromTimeStamp(subAwardOrgService?.subAwdRiskAssmtDate);
            this.subAwardOrg.subAwardOrgFields.subAwdRiskAssmtDate = subAwardOrgService?.subAwdRiskAssmtDate;
        }
    }

    private autoSaveAPI(): void {
        if (!this.isSaving) {
            const TEMP_AUTO_SAVE_RO: SubAwardOrgUpdateClass = deepCloneObject(this.getAutoSaveRO());
            this.addFeedStatusInRO(TEMP_AUTO_SAVE_RO);
            if (isEmptyObject(TEMP_AUTO_SAVE_RO) || isEmptyObject(TEMP_AUTO_SAVE_RO.subAwardOrgFields)) {
                return;
            }
            if (!this.entitySubAwardService.entitySubAwardOrganization?.subAwdOrgDetailsResponseDTO?.id) {
                this.saveSubAwardOrganizationDetails(TEMP_AUTO_SAVE_RO);
            } else {
                this.updateSubAwardOrganizationDetails(TEMP_AUTO_SAVE_RO);
            }
        }
    }

    private getAutoSaveRO(): SubAwardOrgUpdateClass {
        const AUTO_SAVE_PAYLOAD = new SubAwardOrgUpdateClass();
        AUTO_SAVE_PAYLOAD.entityId = this.entityDetails.entityId;
        Object.keys(this.changeDetectionObj).forEach((ele) => {
            if (this.changeDetectionObj[ele]) {
                AUTO_SAVE_PAYLOAD.subAwardOrgFields[ele] = this.subAwardOrg.subAwardOrgFields[ele];
            }
        });
        return AUTO_SAVE_PAYLOAD;
    }

    private saveSubAwardOrganizationDetails(autoSaveReaObj: SubAwardOrgUpdateClass): void {
        this.setupBeforeAPICall();
        this.setChangesObject(autoSaveReaObj.subAwardOrgFields, false);
        this.isSaving = true;
        this.commonService.setLoaderRestriction();
        this.$subscriptions.push(
            this.entitySubAwardService.organizationDetailsAutoSave(autoSaveReaObj)
                .subscribe((data: any) => {
                    this.entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO.entityId = this.entityDetails.entityId;
                    this.entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO.id = data?.id;
                    this.isSaving = false;
                    this.autoSaveAPI();
                    this.handleAPISuccess(autoSaveReaObj);
                }, (_error: any) => {
                    this.isSaving = false;
                    this.handleAPIError(autoSaveReaObj);
                })
        );
        this.commonService.removeLoaderRestriction();
    }

    private updateSubAwardOrganizationDetails(autoSaveReaObj: SubAwardOrgUpdateClass): void {
        this.setupBeforeAPICall();
        this.setChangesObject(autoSaveReaObj.subAwardOrgFields, false);
        this.commonService.setLoaderRestriction();
        this.$subscriptions.push(
            this.entitySubAwardService.updateOrganizationDetails(autoSaveReaObj)
                .subscribe((data: any) => {
                    this.handleAPISuccess(autoSaveReaObj);
                }, (_error: any) => {
                    this.handleAPIError(autoSaveReaObj);
                })
        );
        this.commonService.removeLoaderRestriction();
    }

    private setupBeforeAPICall(): void {
        removeToast('SUCCESS');
        removeToast('ERROR');
        this.commonService.autoSaveSavingLoader = 'SHOW';
    }

    private handleAPISuccess(autoSaveReqObj: SubAwardOrgUpdateClass): void {
        this._dataStoreService.enableModificationHistoryTracking();
        this.updateCompletionFlag();
        this.updateEntireFeed(autoSaveReqObj);
        this.setServiceVariable(autoSaveReqObj.subAwardOrgFields);
        this.commonService.setChangesAvailable(false);
        this.stopAutoSaveLoader('SUCCESS');
    }

    private stopAutoSaveLoader(toastType: 'SUCCESS' | 'ERROR'): void {
        setTimeout(() => {
            if (!this.commonService.loaderRestrictedUrls.length) {
                this.commonService.autoSaveSavingLoader = 'HIDE';
                showEntityToast(toastType);
            }
        });
    }

    private handleAPIError(autoSaveReqObj: SubAwardOrgUpdateClass): void {
        this.setChangesObject(autoSaveReqObj.subAwardOrgFields, true);
        this.commonService.isShowLoader.next(false);
        this.stopAutoSaveLoader('ERROR');
    }

    private setChangesObject(autoSaveReqObj: SubawardOrgFields, isChangesAvailable: boolean): void {
        Object.keys(autoSaveReqObj).forEach((ele) => {
            this.changeDetectionObj[ele] = isChangesAvailable;
        });
    }

    private setServiceVariable(subAwardOrgFields: SubawardOrgFields): void {
        const SUB_AWARD_DTO = this.entitySubAwardService.entitySubAwardOrganization.subAwdOrgDetailsResponseDTO;
        SUB_AWARD_DTO.entityOrganizationType ??= new EntityOrganizationType();
        Object.entries(subAwardOrgFields).forEach(([key, value]) => {
            key === 'organizationTypeCode'
                ? SUB_AWARD_DTO.entityOrganizationType.organizationTypeCode = value
                : SUB_AWARD_DTO[key] = value;
        });
    }

    private addFeedStatusInRO(autoSaveReqObj: SubAwardOrgUpdateClass): void {
        if (this.canUpdateFeed(autoSaveReqObj.subAwardOrgFields)) {
            autoSaveReqObj.subAwardOrgFields.feedStatusCode = '2';
        }
    }

    private updateEntireFeed(autoSaveReqObj: SubAwardOrgUpdateClass): void {
        if (this.canUpdateFeed(autoSaveReqObj.subAwardOrgFields)) {
            this._dataStoreService.updateFeedStatus(this.entityTabStatus, 'ORG');
        }
    }

    private canUpdateFeed(subAwardOrgFields: SubawardOrgFields): boolean {
        return this.entityDetails.entityStatusTypeCode === ENTITY_VERIFICATION_STATUS.VERIFIED && canUpdateOrgFeed(subAwardOrgFields)
            && isOrganizationConditionSatisfied(this.entitySubAwardService.entitySubAwardOrganization);
    }

    onDateSelect(dateType: 'samExpirationDate' | 'subAwdRiskAssmtDate'): void {
        this.commonService.setChangesAvailable(true);
        const SELECTED_DATE = dateType === 'samExpirationDate' ? this.samExpirationDate : this.subAwdRiskAssmtDate;
        this.subAwardOrg.subAwardOrgFields[dateType] = parseDateWithoutTimestamp(SELECTED_DATE);
        this.changeEvent(dateType);
    }

    private changeEvent(key: string): void {
        this.commonService.setChangesAvailable(true);
        this.changeDetectionObj[key] = true;
        this.$debounceEvent.next(key);
    }

    onOrganizationTypeSelect(event: any): void {
        this.subAwardOrg.subAwardOrgFields.organizationTypeCode = event?.[0]?.code || null;
        this.changeEvent('organizationTypeCode');
    }

    private updateCompletionFlag(): void {
        if (isOrganizationConditionSatisfied(this.entitySubAwardService.entitySubAwardOrganization)) {
            this.entityTabStatus.entity_sub_org_info = true;
            this._dataStoreService.updateStore(['entityTabStatus'], { 'entityTabStatus': this.entityTabStatus });
        }
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

}
