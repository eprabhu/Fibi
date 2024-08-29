import { Component, Input, OnInit } from '@angular/core';
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
export class EntitySponsorDetailsComponent implements OnInit {

    @Input() sectionName: any;
    @Input() sectionId: any;
    sponsorDetailsObj: SponsorDetails = new SponsorDetails();
    notificationTemplates = [];
    entitySponsorRiskLevelOption = 'SPONSOR_TYPE#SPONSOR_TYPE_CODE#false#false';
    defaultTemplateLabel = '';
    entityDetails: any;
    $subscriptions: Subscription[] = [];
    isSponsorDetailsFormChanged: boolean;
    autoSaveRO: any = {};
    $debounceEvent = new Subject<any>();



    constructor(private _dataStoreService: EntityDataStoreService,
                private _autoSaveService: AutoSaveService,
                private _entityManagementService: EntityManagementService,
                private _entitySponsorService: EntitySponsorService,
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
        console.log("ent details",this.entityDetails);
        this.setOtherDetailsObject();
        
    }

    triggerSingleSave() {
        console.log("entered trigger single save");

        this.$subscriptions.push(this.$debounceEvent.pipe(debounce(() => interval(2000))).subscribe((data: any) => {
          if (data) {
            this._autoSaveService.commonSaveTrigger$.next(true);
          }
        }
        ));
    }

    setOtherDetailsObject(){
        this.sponsorDetailsObj.acronym = this.entityDetails.acronym;
        this.sponsorDetailsObj.sponsorTypeCode = this.entityDetails.sponsorTypeCode;
    }

    onSponsorTypeSelect(event){
        if(event){
            this.sponsorDetailsObj.sponsorTypeCode = event[0]?.code;
            this.changeEvent('acronym');
        }
        else{
            this.sponsorDetailsObj.sponsorTypeCode = null;
        }
    }

    autoSaveSubscribe() {
        console.log("entered auto save subscribe");
        this.$subscriptions.push(this._autoSaveService.autoSaveTrigger$.subscribe(event => this.autoSaveAPI()));
        console.log("reachout auto save subscribe");

    }

    changeEvent(key) {
        console.log("changes occuredsposnor");
        this.isSponsorDetailsFormChanged = true;
        this._entityManagementService.hasChangesAvailable = true;
        if(this.sponsorDetailsObj[key]) {
            this.sponsorDetailsObj[key] = this.sponsorDetailsObj[key].trim();
            this.autoSaveRO[key] = this.sponsorDetailsObj[key];
            this.$debounceEvent.next(true);
        }
    }

    autoSaveAPI() {
        console.log("entered auto save api");

        if(this.isSponsorDetailsFormChanged) {
                this.addOtherDetailsAPI();
            // }
        }
    }

    addOtherDetailsAPI() {
        console.log("entered otherdetailsApi");

        if(Object.keys(this.autoSaveRO).length) {
            this.autoSaveRO.entityId = this.entityDetails.entityId;
            this.commonService.setLoaderRestriction();
            this.$subscriptions.push(this._entitySponsorService.updateSponsorDetails(this.autoSaveRO).subscribe((data) => {
                this.autoSaveRO = {};
                this._entityManagementService.hasChangesAvailable = false;
                this.isSponsorDetailsFormChanged = false;
                showEntityToast('SUCCESS');
                console.log("api call finished and successfully updated");

            }, err => {
                console.log(err);
                showEntityToast('ERROR');
                console.log("api call finished and failed");

            }));
            this.commonService.removeLoaderRestriction();
        }
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }
}
