import {Component, Input, OnDestroy, OnInit} from '@angular/core';
import {EntityTabStatus, showEntityToast, SponsorDetails, SponsorType} from '../../shared/entity-interface';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { interval, Subject, Subscription } from 'rxjs';
import { AutoSaveService } from '../../../common/services/auto-save.service';
import { canUpdateSponsorFeed, EntityManagementService } from '../../entity-management.service';
import { EntitySponsorService } from '../entity-sponsor.service';
import { CommonService } from '../../../common/services/common.service';
import { debounce } from 'rxjs/operators';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { ENTITY_VERIFICATION_STATUS } from '../../../app-constants';

@Component({
    selector: 'app-entity-sponsor-details',
    templateUrl: './entity-sponsor-details.component.html',
    styleUrls: ['./entity-sponsor-details.component.scss']
})
export class EntitySponsorDetailsComponent implements OnInit, OnDestroy {

    @Input() sectionName: any;
    @Input() sectionId: any;
    // @Input() entitySponsorService.entitySponsorDetails: any;
    sponsorDetailsObj: SponsorDetails = new SponsorDetails();
    notificationTemplates = [];
    entitySponsorTypeOption = 'SPONSOR_TYPE#SPONSOR_TYPE_CODE#false#false';
    defaultTemplateLabel = '';
    entityDetails: any;
    $subscriptions: Subscription[] = [];
    dataChangeCounter = 0;
    autoSaveRO: any = {};
    $debounceEvent = new Subject<any>();
    isRestrictSave = false;
    isEditMode = false;
    entityTabStatus: EntityTabStatus = new EntityTabStatus();

    constructor(private _dataStoreService: EntityDataStoreService,
                private _autoSaveService: AutoSaveService,
                private _entityManagementService: EntityManagementService,
                public entitySponsorService: EntitySponsorService,
                public commonService: CommonService,
    ) { }

    ngOnInit() {
        this.triggerSingleSave();
        this.getDataFromStore();
        this.autoSaveSubscribe();
        this.listenDataChangeFromStore();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private getDataFromStore() {
        const entityData = this._dataStoreService.getData();
        if (isEmptyObject(entityData)) { return; }
        this.entityDetails = entityData?.entityDetails;
        this.setOtherDetailsObject();
        this.isEditMode = this._dataStoreService.getEditMode();
        this.entityTabStatus = entityData?.entityTabStatus;
        this.checkUserHasRight();
    }

    triggerSingleSave() {
        this.$subscriptions.push(this.$debounceEvent.pipe(debounce(() => interval(2000))).subscribe((data: any) => {
          if (data) {
            this.dataChangeCounter++;
            this._autoSaveService.commonSaveTrigger$.next(true);
          }
        }
        ));
    }

    setOtherDetailsObject() {
        const sponsorDetails = this.entitySponsorService.entitySponsorDetails?.sponsorDetailsResponseDTO;
        this.sponsorDetailsObj.acronym = sponsorDetails?.acronym ?? '';
        this.sponsorDetailsObj.sponsorType = sponsorDetails?.sponsorType ?? new SponsorType();
        this.sponsorDetailsObj.sponsorCode = sponsorDetails?.sponsorCode ?? '';
    }

    onSponsorTypeSelect(event) {
        if (event) {
            this.sponsorDetailsObj.sponsorType.code = event[0]?.code;
            this.sponsorDetailsObj.sponsorType.description = event[0]?.description;
            this.changeEvent('sponsorTypeCode');
        } else {
            this.sponsorDetailsObj.sponsorType = null;
        }
    }

    autoSaveSubscribe() {
        this.$subscriptions.push(this._autoSaveService.autoSaveTrigger$.subscribe(event => this.autoSaveAPI()));
    }

    changeEvent(key) {
        this.commonService.setChangesAvailable(true);
        const IS_SPONSOR_TYPE = key === 'sponsorTypeCode';
        if(this.sponsorDetailsObj[key] || IS_SPONSOR_TYPE) {
            if (IS_SPONSOR_TYPE) {
                this.autoSaveRO['sponsorTypeCode'] = this.sponsorDetailsObj.sponsorType.code;
            } else {
                this.sponsorDetailsObj[key] = this.sponsorDetailsObj[key].trim();
                this.autoSaveRO[key] = this.sponsorDetailsObj[key];
            }
            this.$debounceEvent.next(true);
        }
    }

    autoSaveAPI() {
        if (this.dataChangeCounter > 0 && !this.isRestrictSave) {
            if (!this.entitySponsorService.entitySponsorDetails?.sponsorDetailsResponseDTO?.id) {
                this.isRestrictSave = true;
                this.addNewDetail();
            }
            else {
                this.addOtherDetailsAPI();
            }
        }
    }

    addNewDetail(){
        if(Object.keys(this.autoSaveRO).length) {
            this.commonService.setLoaderRestriction();
            this.autoSaveRO.entityId = this.entityDetails.entityId;
            this.addFeedStatusInRO();
            this.$subscriptions.push(this.entitySponsorService.SponsorDetailsAutoSave(this.autoSaveRO).subscribe((data: any) => {
                this.dataChangeCounter--;
                this._dataStoreService.enableModificationHistoryTracking();
                this.entitySponsorService.entitySponsorDetails.sponsorDetailsResponseDTO = {
                    id: data
                }
                if(this.autoSaveRO.acronym) {
                    this.entitySponsorService.entitySponsorDetails.sponsorDetailsResponseDTO.acronym = this.autoSaveRO.acronym;
                    this.sponsorDetailsObj.acronym = this.autoSaveRO.acronym;
                }
                if(this.autoSaveRO.sponsorTypeCode) {
                    this.entitySponsorService.entitySponsorDetails.sponsorDetailsResponseDTO.sponsorCode = this.autoSaveRO.sponsorTypeCode;
                    this.sponsorDetailsObj.sponsorCode = this.autoSaveRO.sponsorTypeCode;
                }
                showEntityToast('SUCCESS');
                if (this.autoSaveRO.hasOwnProperty('sponsorTypeCode') && this.autoSaveRO?.sponsorTypeCode) {
                    this.updateSponsorCompleteFlag();
                }
                this.updateEntireFeed();
                this.autoSaveRO = {};
                this.isRestrictSave = false;
                this.commonService.setChangesAvailable(false);
            }, err => {
                showEntityToast('ERROR');
                this.isRestrictSave = false;
            }));
            this.commonService.removeLoaderRestriction();
        }
    }
    private updateEntireFeed(): void {
        if(this.canUpdateFeed()) {
           this._dataStoreService.updateFeedStatus(this.entityTabStatus, 'SPONSOR');
        }
    }

    private addFeedStatusInRO(): void {
        if(this.canUpdateFeed()) {
            this.autoSaveRO.feedStatusCode = '2';
        }
    }

    canUpdateFeed(): boolean {
        return (this.entityDetails.entityStatusTypeCode === ENTITY_VERIFICATION_STATUS.VERIFIED && canUpdateSponsorFeed(this.autoSaveRO) &&
               (this.autoSaveRO.hasOwnProperty('sponsorTypeCode') && this.autoSaveRO?.sponsorTypeCode));
    }

    addOtherDetailsAPI() {
        if(Object.keys(this.autoSaveRO).length) {
            this.setDetailsForUpdate();
            this.commonService.setLoaderRestriction();
            this.addFeedStatusInRO();
            this.$subscriptions.push(this.entitySponsorService.updateSponsorDetails(this.autoSaveRO).subscribe((data) => {
                this.dataChangeCounter--;
                this._dataStoreService.enableModificationHistoryTracking();
                showEntityToast('SUCCESS');
                if (this.autoSaveRO.acronym) {
                    this.entitySponsorService.entitySponsorDetails.sponsorDetailsResponseDTO.acronym = this.autoSaveRO.acronym;
                }
                if (this.autoSaveRO.sponsorTypeCode) {
                    this.entitySponsorService.entitySponsorDetails.sponsorDetailsResponseDTO.sponsorType = this.sponsorDetailsObj.sponsorType;
                }
                if (this.autoSaveRO.hasOwnProperty('sponsorTypeCode') && this.autoSaveRO?.sponsorTypeCode) {
                    this.updateSponsorCompleteFlag();
                }
                this.updateEntireFeed();
                this.autoSaveRO = {};
                this.commonService.setChangesAvailable(false);
            }, err => {
                showEntityToast('ERROR');
            }));
            this.commonService.removeLoaderRestriction();
        }
    }

    updateSponsorCompleteFlag() {
        this.entityTabStatus.entity_sponsor_info = true;
        this._dataStoreService.updateStore(['entityTabStatus'], { 'entityTabStatus':  this.entityTabStatus });
    }

    setDetailsForUpdate(){
        this.autoSaveRO.entityId = this.entityDetails.entityId;
        this.autoSaveRO.sponsorTypeCode = this.sponsorDetailsObj.sponsorType.code;
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    checkUserHasRight(): void {
        const hasRight = this.commonService.getAvailableRight(['MANAGE_ENTITY_SPONSOR'], 'SOME');
        if(!hasRight) {
            this.isEditMode = false;
        }
    }
}
