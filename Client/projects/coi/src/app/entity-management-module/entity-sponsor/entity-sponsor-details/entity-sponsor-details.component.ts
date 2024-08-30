import {Component, Input, OnDestroy, OnInit} from '@angular/core';
import { showEntityToast, SponsorDetails } from '../../shared/entity-interface';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { interval, Subject, Subscription } from 'rxjs';
import { AutoSaveService } from '../../../common/services/auto-save.service';
import { EntityManagementService } from '../../entity-management.service';
import { EntitySponsorService } from '../entity-sponsor.service';
import { CommonService } from '../../../common/services/common.service';
import { debounce } from 'rxjs/operators';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';

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

    setOtherDetailsObject(){
        this.sponsorDetailsObj.acronym = this.entitySponsorService.entitySponsorDetails?.sponsorDetailsResponseDTO?.acronym ? this.entitySponsorService.entitySponsorDetails?.sponsorDetailsResponseDTO?.acronym : '';
        this.sponsorDetailsObj.sponsorTypeCode = this.entitySponsorService.entitySponsorDetails?.sponsorDetailsResponseDTO?.sponsorTypeCode ? this.entitySponsorService.entitySponsorDetails?.sponsorDetailsResponseDTO?.sponsorTypeCode : '';
        this.sponsorDetailsObj.sponsorCode = this.entitySponsorService.entitySponsorDetails?.sponsorDetailsResponseDTO?.sponsorCode ? this.entitySponsorService.entitySponsorDetails?.sponsorDetailsResponseDTO?.sponsorCode : '';
    }

    onSponsorTypeSelect(event){
        if(event){
            this.sponsorDetailsObj.sponsorTypeCode = event[0]?.code;
            this.changeEvent('sponsorTypeCode');
        }
        else{
            this.sponsorDetailsObj.sponsorTypeCode = null;
        }
    }

    autoSaveSubscribe() {
        this.$subscriptions.push(this._autoSaveService.autoSaveTrigger$.subscribe(event => this.autoSaveAPI()));
    }

    changeEvent(key) {
        this._entityManagementService.hasChangesAvailable = true;
        if(this.sponsorDetailsObj[key]) {
            this.sponsorDetailsObj[key] = this.sponsorDetailsObj[key].trim();
            this.autoSaveRO[key] = this.sponsorDetailsObj[key];
            this.$debounceEvent.next(true);
        }
    }

    autoSaveAPI() {
        if (this.dataChangeCounter > 0 && !this.isRestrictSave) {
            if (!this.entitySponsorService.entitySponsorDetails?.sponsorDetailsResponseDTO) {
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
            this.$subscriptions.push(this.entitySponsorService.SponsorDetailsAutoSave(this.autoSaveRO).subscribe((data) => {
                this._entityManagementService.hasChangesAvailable = false;
                this.dataChangeCounter--;
                showEntityToast('SUCCESS');
                this.entitySponsorService.entitySponsorDetails.sponsorDetailsResponseDTO = this.autoSaveRO;
                this.sponsorDetailsObj = this.autoSaveRO;
                this.autoSaveRO = {};
                this.isRestrictSave = false;
            }, err => {
                showEntityToast('ERROR');
                this.isRestrictSave = false;
            }));
            this.commonService.removeLoaderRestriction();
        }
    }

    addOtherDetailsAPI() {
        if(Object.keys(this.autoSaveRO).length) {
            this.setDetailsForUpdate();
            this.commonService.setLoaderRestriction();
            this.$subscriptions.push(this.entitySponsorService.updateSponsorDetails(this.autoSaveRO).subscribe((data) => {
                this._entityManagementService.hasChangesAvailable = false;
                this.dataChangeCounter--;
                showEntityToast('SUCCESS');
                this.entitySponsorService.entitySponsorDetails.sponsorDetailsResponseDTO.sponsorTypeCode = this.autoSaveRO.sponsorTypeCode;
                this.autoSaveRO = {};
            }, err => {
                showEntityToast('ERROR');
            }));
            this.commonService.removeLoaderRestriction();
        }
    }

    setDetailsForUpdate(){
        this.autoSaveRO.entityId = this.entityDetails.entityId;
        this.autoSaveRO.sponsorTypeCode = this.sponsorDetailsObj.sponsorTypeCode
        this.entitySponsorService.entitySponsorDetails.sponsorDetailsResponseDTO = this.autoSaveRO;

    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }
}
