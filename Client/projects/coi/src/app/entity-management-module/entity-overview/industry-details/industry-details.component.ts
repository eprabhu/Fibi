import { Component } from '@angular/core';
import { deepCloneObject, hideModal, isEmptyObject, openModal } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { IndustryDetails } from '../../shared/entity-interface';
import { Subscription } from 'rxjs';
import { EntityOverviewService } from '../entity-overview.service';
import { EntityDataStoreService } from '../../entity-data-store.service';

@Component({
  selector: 'app-industry-details',
  templateUrl: './industry-details.component.html',
  styleUrls: ['./industry-details.component.scss']
})
export class IndustryDetailsComponent {

    industryDetails: IndustryDetails = new IndustryDetails();
    industryCategoryTypeOptions = 'INDUSTRY_CATEGORY_TYPE#INDUSTRY_CATEGORY_TYPE_CODE#false#false';
    industryCategoryDescriptionOptions = 'EMPTY#EMPTY#true#true';
    mandatoryList = new Map();
    $subscriptions: Subscription[] = [];
    industryCategoryTypeCode: any;
    entityIndustryCategories = [];
    industryCategoryDescriptionsList: any = [];
    entityId: any;
    entityIndustryClassifications: any = [];
    entityIndustryClassificationsGrouping: any = {};
    categoryDescriptionList = [];
    categoryTypeList = [];

    addIndustryDetails(event) {
        if(event) {
            openModal('addIndustryDetails');
        }
    }

    constructor(private _entityOverviewService: EntityOverviewService, private _dataStoreService: EntityDataStoreService) {}

    ngOnInit(){
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    clearIndustryDetails() {
        this.mandatoryList.clear();
        this.industryDetails = new IndustryDetails();
        this.categoryTypeList = [];
        this.categoryDescriptionList = [];
        this.industryCategoryTypeCode = null;
        this.entityIndustryCategories = [];
        hideModal('addIndustryDetails');
    }

    addIndustry() {
        this.entityMandatoryValidation();
        if(!this.mandatoryList.size) {
            this.saveIndustryDetails();
        }
    }

    onIndustryCategoryTypeSelect(event) {
        this.industryCategoryDescriptionsList = [];
        if(event && event.length) {
            this.industryCategoryTypeCode = event[0].code;
            this.fetchIndustryDescription();
        } else {
            this.industryCategoryTypeCode = null;
        }
    }

    fetchIndustryDescription() {
        this.$subscriptions.push(this._entityOverviewService.fetchIndustryCategoryCode(this.industryCategoryTypeCode).subscribe((data: any) => {
            if(data) {
                this.industryCategoryDescriptionsList = deepCloneObject(data);
            }
        }))
    }


    entityMandatoryValidation(): void {
        this.mandatoryList.clear();
        if(!this.industryCategoryTypeCode || !this.industryCategoryTypeCode.length) {
            this.mandatoryList.set('industryCategoryTypeCode', 'Please select industry category type.');
        }
        if(!this.entityIndustryCategories || !this.entityIndustryCategories.length) {
            this.mandatoryList.set('industryCategroyDescription', 'Please select industry category description.');
        }
    }

    onIndustryCategoryDescriptionSelect(event) {
        if(event && event.length) {
            this.entityIndustryCategories = event;
        } else {
            this.entityIndustryCategories = [];
        }
    }

    saveIndustryDetails() {
        this.setReqObj();
        this.$subscriptions.push(this._entityOverviewService.saveIndustryDetails(this.industryDetails).subscribe((data: any) => {
            this._dataStoreService.updateStore(['entityIndustryClassifications'], { 'entityIndustryClassifications':  data });
            this.clearIndustryDetails();
        }))
    }

    setReqObj() {
        this.industryDetails.entityIndustryCatIds = [];
        this.industryDetails.entityId = this.entityId;
        this.entityIndustryCategories.forEach((ele) => {
            this.industryDetails.entityIndustryCatIds.push(ele.industryCategoryId);
        })
    }

    private getDataFromStore() {
        const entityData = this._dataStoreService.getData();
        if (isEmptyObject(entityData)) { return; }
        this.entityId = entityData?.entityDetails?.entityId;
        this.entityIndustryClassifications = entityData.entityIndustryClassifications;
        if(this.entityIndustryClassifications.length) {
            this.entityIndustryClassificationsGrouping = this.groupBy(this.entityIndustryClassifications, 'industryCategoryCode', 'industryCategoryType', 'description');
        }
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[] | 'ENTITY_RISK_TYPE') => {
                if (dependencies !==  'ENTITY_RISK_TYPE') {
                    this.getDataFromStore();
                }
            })
        );
    }

    groupBy(jsonData, key, innerKey, secondInnerKey) {
        return jsonData.reduce((relationsTypeGroup, item) => {
            (relationsTypeGroup[item[key][innerKey][secondInnerKey]] = relationsTypeGroup[item[key][innerKey][secondInnerKey]] || []).push(item);
            return relationsTypeGroup;
        }, {});
    }

    editIndustry(classification) {
        console.log(classification);
    }

}
