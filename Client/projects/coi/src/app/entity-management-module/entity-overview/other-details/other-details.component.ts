import { Component } from '@angular/core';
import { EntityDetails, OtherDetails } from '../../shared/entity-interface';
import { getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../../common/utilities/date-utilities';
import { CommonService } from '../../../common/services/common.service';
import { DATE_PLACEHOLDER } from '../../../app-constants';
import { interval, Subject, Subscription } from 'rxjs';
import { debounce } from 'rxjs/operators';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { EntityOverviewService } from '../entity-overview.service';

@Component({
  selector: 'app-other-details',
  templateUrl: './other-details.component.html',
  styleUrls: ['./other-details.component.scss']
})
export class OtherDetailsComponent {

    entityPriorName: string;
    entityAlternateName: string;
    startDate: any;
    incorporationDate:any;
    otherDetailsObj: OtherDetails = new OtherDetails();
    datePlaceHolder = DATE_PLACEHOLDER;
    coiCurrencyOptions = 'currency#CURRENCY_CODE#true#true';
    $debounceEvent = new Subject<any>();
    $subscriptions: Subscription[] = [];
    isFormDataChanged = false;
    autoSaveRO: any = {};
    entityDetails: EntityDetails = new EntityDetails();

    constructor(public commonService: CommonService, private _entityOverviewService: EntityOverviewService,
        public dataStore: EntityDataStoreService,
    ) {}

    ngOnInit() {
        this.triggerSingleSave();
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    private getDataFromStore() {
        const entityData = this.dataStore.getData();
        if (!entityData || isEmptyObject(entityData)) { return; }
        this.entityDetails = entityData.entityDetails;
        this.setOtherDetailsObject();
    }

    setOtherDetailsObject() {
        this.startDate = getDateObjectFromTimeStamp(this.entityDetails?.startDate);
        this.incorporationDate = getDateObjectFromTimeStamp(this.entityDetails?.incorporationDate);
        this.otherDetailsObj.activityText = this.entityDetails?.activityText;
        // this.otherDetailsObj.congressionalDistrict = this.entityDetails?.

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
            this.otherDetailsObj.currencyCode = event;
        } else {
            this.otherDetailsObj.currencyCode = null;
        }
    }

    triggerSingleSave() {
        this.$subscriptions.push(this.$debounceEvent.pipe(debounce(() => interval(2000))).subscribe((data: any) => {
          if (data) {
            this.autoSaveAPI();
          }
        }
        ));
      }

    changeEvent(key) {
        this.isFormDataChanged = true;
        if(this.otherDetailsObj[key]) {
            this.autoSaveRO[key] = this.otherDetailsObj[key];
            this.$debounceEvent.next(true);
        }
    }

    autoSaveAPI() {
        this._entityOverviewService.updateOtherDetails(this.autoSaveRO);
        this.autoSaveRO = {};
    }
    addEntityPriorName() {
        this.otherDetailsObj.entityPriorName.push(this.entityPriorName);
        this.changeEvent('entityPriorName');
        this.entityPriorName = '';
    }

    deletePriorName(index) {
        this.otherDetailsObj.entityPriorName.splice(index, 1);
    }

    addEntityAlternatename() {
        this.otherDetailsObj.entityAlternateName.push(this.entityAlternateName);
        this.changeEvent('entityAlternateName');
        this.entityAlternateName = '';
    }

    deleteEntityAlternateName(index) {
        this.otherDetailsObj.entityAlternateName.splice(index, 1);
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

}
