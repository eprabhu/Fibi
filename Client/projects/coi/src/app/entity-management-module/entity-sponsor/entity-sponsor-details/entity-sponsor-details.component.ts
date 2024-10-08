import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { EntireEntityDetails, EntityTabStatus, removeToast, showEntityToast, SponsorDetails, SponsorFields, SponsorType, SponsorUpdateClass } from '../../shared/entity-interface';
import { deepCloneObject, isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { interval, Subject, Subscription } from 'rxjs';
import { AutoSaveService } from '../../../common/services/auto-save.service';
import { canUpdateSponsorFeed } from '../../entity-management.service';
import { EntitySponsorService } from '../entity-sponsor.service';
import { CommonService } from '../../../common/services/common.service';
import { debounce } from 'rxjs/operators';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { ENTITY_VERIFICATION_STATUS } from '../../../app-constants';
import { AUTO_SAVE_DEBOUNCE_TIME } from '../../shared/entity-constants';

@Component({
    selector: 'app-entity-sponsor-details',
    templateUrl: './entity-sponsor-details.component.html',
    styleUrls: ['./entity-sponsor-details.component.scss']
})
export class EntitySponsorDetailsComponent implements OnInit, OnDestroy {

    @Input() sectionName: any;
    @Input() sectionId: any;
    SPONSOR_TYPE_OPTIONS = 'SPONSOR_TYPE#SPONSOR_TYPE_CODE#false#false';
    entityDetails: any;
    $subscriptions: Subscription[] = [];
    $debounceEvent = new Subject<any>();
    isEditMode = false;
    entityTabStatus: EntityTabStatus = new EntityTabStatus();
    selectedLookupList: any[] = [];
    sponsorUpdateObj = new SponsorUpdateClass();
    changeDetectionObj = new SponsorFields();
    isSaving = false;

    constructor(private _dataStoreService: EntityDataStoreService,
        private _autoSaveService: AutoSaveService,
        public entitySponsorService: EntitySponsorService,
        public commonService: CommonService,
    ) { }

    ngOnInit() {
        this.triggerSingleSave();
        this.autoSaveSubscribe();
        this.listenDataChangeFromStore();
        this.getDataFromStore();
        this.getValuesFromService(deepCloneObject(this.entitySponsorService.entitySponsorDetails.sponsorDetailsResponseDTO));
    }

    private triggerSingleSave(): void {
        this.$subscriptions.push(this.$debounceEvent.pipe(debounce(() => interval(AUTO_SAVE_DEBOUNCE_TIME))).subscribe((data: any) => {
            if (data) {
                this._autoSaveService.commonSaveTrigger$.next(true);
            }
        }));
    }

    private autoSaveSubscribe(): void {
        this.$subscriptions.push(this._autoSaveService.autoSaveTrigger$.subscribe((event: any) => {
            this.autoSaveAPI();
        }));
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private getValuesFromService(sponsorDetails: SponsorDetails): void {
        if (sponsorDetails?.sponsorType?.code) {
            this.selectedLookupList.push({ code: sponsorDetails?.sponsorType?.code });
            this.sponsorUpdateObj.entitySponsorFields.sponsorCode = sponsorDetails?.sponsorType?.code;
        }
        this.sponsorUpdateObj.entitySponsorFields.acronym = sponsorDetails?.acronym;
        this.sponsorUpdateObj.entitySponsorFields.sponsorCode = sponsorDetails?.sponsorCode;
    }

    private getDataFromStore(): void {
        const ENTITY_DATA: EntireEntityDetails = this._dataStoreService.getData();
        if (isEmptyObject(ENTITY_DATA)) { return; }
        this.entityDetails = ENTITY_DATA?.entityDetails;
        this.entityTabStatus = ENTITY_DATA?.entityTabStatus;
        this.checkUserHasRight();
    }

    private checkUserHasRight(): void {
        this.isEditMode = this._dataStoreService.getEditMode() && this.commonService.getAvailableRight(['MANAGE_ENTITY_SPONSOR'], 'SOME');
    }

    private autoSaveAPI(): void {
        if (!this.isSaving) {
            const TEMP_AUTO_SAVE_RO: SponsorUpdateClass = deepCloneObject(this.getAutoSaveRO());
            this.addFeedStatusInRO(TEMP_AUTO_SAVE_RO);
            if (isEmptyObject(TEMP_AUTO_SAVE_RO) || isEmptyObject(TEMP_AUTO_SAVE_RO.entitySponsorFields)) {
                return;
            }
            if (!this.entitySponsorService.entitySponsorDetails?.sponsorDetailsResponseDTO?.id) {
                this.saveSponsorDetails(TEMP_AUTO_SAVE_RO);
            } else {
                this.updateSponosrDetails(TEMP_AUTO_SAVE_RO);
            }
        }
    }

    private getAutoSaveRO(): SponsorUpdateClass {
        const AUTO_SAVE_PAYLOAD = new SponsorUpdateClass();
        AUTO_SAVE_PAYLOAD.entityId = this.entityDetails.entityId;
        Object.keys(this.changeDetectionObj).forEach((ele) => {
            if (this.changeDetectionObj[ele]) {
                AUTO_SAVE_PAYLOAD.entitySponsorFields[ele] = this.sponsorUpdateObj?.entitySponsorFields[ele]?.trim() || null;
            }
        });
        return AUTO_SAVE_PAYLOAD;
    }

    changeEvent(key: string): void {
        this.commonService.setChangesAvailable(true);
        this.changeDetectionObj[key] = true;
        this.$debounceEvent.next(key);
    }

    private saveSponsorDetails(autoSaveReaObj: SponsorUpdateClass): void {
        this.setupBeforeAPICall();
        this.setChangesObject(autoSaveReaObj.entitySponsorFields, false);
        this.isSaving = true;
        this.commonService.setLoaderRestriction();
        this.$subscriptions.push(
            this.entitySponsorService.sponsorDetailsAutoSave(autoSaveReaObj)
                .subscribe((data: any) => {
                    this.entitySponsorService.entitySponsorDetails.sponsorDetailsResponseDTO ??= new SponsorDetails();
                    this.entitySponsorService.entitySponsorDetails.sponsorDetailsResponseDTO.entityId = this.entityDetails.entityId;
                    this.entitySponsorService.entitySponsorDetails.sponsorDetailsResponseDTO.id = data?.id;
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

    private addFeedStatusInRO(autoSaveReqObj: SponsorUpdateClass): void {
        if (this.canUpdateFeed(autoSaveReqObj.entitySponsorFields)) {
            autoSaveReqObj.entitySponsorFields.feedStatusCode = '2';
        }
    }

    private canUpdateFeed(entitySponsorFields: SponsorFields): boolean {
        return (this.entityDetails.entityStatusTypeCode === ENTITY_VERIFICATION_STATUS.VERIFIED && canUpdateSponsorFeed(entitySponsorFields)
            && (entitySponsorFields.sponsorTypeCode || this.entitySponsorService?.entitySponsorDetails?.sponsorDetailsResponseDTO?.sponsorType?.code));
    }

    private updateSponosrDetails(autoSaveReaObj: SponsorUpdateClass): void {
        this.setupBeforeAPICall();
        this.setChangesObject(autoSaveReaObj.entitySponsorFields, false);
        this.commonService.setLoaderRestriction();
        this.$subscriptions.push(
            this.entitySponsorService.updateSponsorDetails(autoSaveReaObj)
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

    private handleAPISuccess(autoSaveReqObj: SponsorUpdateClass): void {
        this._dataStoreService.enableModificationHistoryTracking();
        this.setServiceVariable(autoSaveReqObj.entitySponsorFields);
        this.updateCompletionFlag();
        this.updateEntireFeed(autoSaveReqObj);
        this.commonService.setChangesAvailable(false);
        this.stopAutoSaveLoader('SUCCESS');
    }

    private setServiceVariable(entitySponsorFields: SponsorFields): void {
        const SUB_AWARD_DTO = this.entitySponsorService?.entitySponsorDetails?.sponsorDetailsResponseDTO;
        SUB_AWARD_DTO.sponsorType ??= new SponsorType();
        Object.entries(entitySponsorFields).forEach(([key, value]) => {
            key === 'sponsorTypeCode'
                ? SUB_AWARD_DTO.sponsorType.code = value
                : SUB_AWARD_DTO[key] = value;
        });
    }
    private stopAutoSaveLoader(toastType: 'SUCCESS' | 'ERROR'): void {
        setTimeout(() => {
            if (!this.commonService.loaderRestrictedUrls.length) {
                this.commonService.autoSaveSavingLoader = 'HIDE';
                showEntityToast(toastType);
            }
        });
    }

    private handleAPIError(autoSaveReqObj: SponsorUpdateClass): void {
        this.setChangesObject(autoSaveReqObj.entitySponsorFields, true);
        this.commonService.isShowLoader.next(false);
        this.stopAutoSaveLoader('ERROR');
    }

    private updateCompletionFlag(): void {
        if (this.entitySponsorService?.entitySponsorDetails?.sponsorDetailsResponseDTO?.sponsorType?.code) {
            this.entityTabStatus.entity_sponsor_info = true;
            this._dataStoreService.updateStore(['entityTabStatus'], { 'entityTabStatus': this.entityTabStatus });
        }
    }

    private setChangesObject(autoSaveReqObj: SponsorFields, isChangesAvailable: boolean): void {
        Object.keys(autoSaveReqObj).forEach((ele) => {
            this.changeDetectionObj[ele] = isChangesAvailable;
        });
    }

    private updateEntireFeed(autoSaveReqObj: SponsorUpdateClass): void {
        if (this.canUpdateFeed(autoSaveReqObj.entitySponsorFields)) {
            this._dataStoreService.updateFeedStatus(this.entityTabStatus, 'SPONSOR');
        }
    }

    onSponsorTypeSelect(event: any): void {
        this.sponsorUpdateObj.entitySponsorFields.sponsorTypeCode = event?.[0]?.code || null;
        this.changeEvent('sponsorTypeCode');
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }
}
