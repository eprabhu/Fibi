import { Component, OnDestroy, OnInit } from '@angular/core';
import { parseDateWithoutTimestamp } from '../../../../fibi/src/app/common/utilities/date-utilities';
import { slowSlideInOut, slideHorizontal, fadeDown, slideInOut } from '../../../../fibi/src/app/common/utilities/animations';
import { EntityDashDefaultValues, EntityDashboardRequest, EntityManagementService } from './entity-management.service';
import { ElasticConfigService } from '../../../../fibi/src/app/common/services/elastic-config.service';
import { Router } from '@angular/router';
import { getEndPointOptionsForCountry, getEndPointOptionsForEntity } from '../../../../fibi/src/app/common/services/end-point.config';
import { Subject, Subscription } from 'rxjs';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../fibi/src/app/app-constants';
import { subscriptionHandler } from '../../../../fibi/src/app/common/utilities/subscription-handler';
import { CommonService } from '../common/services/common.service';
import { switchMap } from 'rxjs/operators';


@Component({
  selector: 'app-entity-management',
  templateUrl: './entity-management.component.html',
  styleUrls: ['./entity-management.component.scss'],
  animations: [slideInOut]
})
export class EntityManagementComponent implements OnInit, OnDestroy {

  entityManageId = null;
  activeTabName = 'ALL_ENTITIES';
  isViewAdvanceSearch = true;
  coiElastic = null;
  isCoiEditEntity = false;
  clearField: String;
  advSearchClearField: String;
  EntitySearchOptions: any = {};
  countrySearchOptions: any = {};
  lookupValues = [];
  riskLevelTypeOptions = 'entity_risk_category#RISK_CATEGORY_CODE#true#true';
  entityTypeOptions = 'entity_type#ENTITY_TYPE_CODE#true#true';
  statusTypeOptions = 'EMPTY#EMPTY#true#true';
  entityList: any = [];
  $subscriptions: Subscription[] = [];
  resultCount: number = 0;
  isSearchData = false;

  constructor(public entityManagementService: EntityManagementService,
    private _elasticConfig: ElasticConfigService, private _router: Router,
    private _commonService:CommonService) {
  }
  ngOnInit() {
    this.coiElastic = this._elasticConfig.getElasticForCoi();
    this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonService.baseUrl);
    this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
    if (!this.entityManagementService.coiRequestObject.tabName) {
      this.entityManagementService.coiRequestObject.tabName = this.activeTabName;
    } else {
      this.activeTabName = this.entityManagementService.coiRequestObject.tabName;
      this.isViewAdvanceSearch = true;
      this.EntitySearchOptions.defaultValue = this.entityManagementService.entityDashDefaultValues.entitySearch;
      this.countrySearchOptions.defaultValue = this.entityManagementService.entityDashDefaultValues.countrySearch;
      this.generateLookupArrayForDropdown();
      this.advancedSearch();
    }

  }
  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions)
    this.entityManagementService.isShowEntityNavBar = false;
  }

  selectedEntity(event) {
    this.entityManageId = event;
    this.isCoiEditEntity = true;
    this.entityManagementService.isShowEntityNavBar = true;
  }

  entityTabName(tabName) {
    this.resetAdvanceSearchFields();
    this.activeTabName = tabName;
    this.entityManagementService.coiRequestObject.tabName = this.activeTabName;
    if(this.activeTabName ==='ALL_ENTITIES'){
      this.isSearchData = false;
      this.isViewAdvanceSearch = true;

    } else {
      this.isSearchData = true;
      this.viewListOfEntity();
      this.isViewAdvanceSearch = false;
    }
  }

  redirectToEntity(coi: any) {
    this._router.navigate(['/coi/entity-management/entity-list'], { queryParams: { entityManageId: coi.id } });
  }

  viewListOfEntity() {
    this.$subscriptions.push(
          this.entityManagementService.getAllSystemEntityList(this.entityManagementService.coiRequestObject)
        .subscribe((res: any) => {
          this.entityList = res.coiEntityList || [];
          this.resultCount = res.entityCount;
        }, _error => {
          this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
          this.entityList = [];
        }));
  }

  addNewEntity() {
    this.entityManageId = null;
    this.isCoiEditEntity = false;
    this.entityManagementService.isShowEntityNavBar = true;
  }

  selectedEvent(event) {
    this.entityManagementService.coiRequestObject.property1 = event ? event.entityName : '';
    this.entityManagementService.entityDashDefaultValues.entitySearch = event ? event.entityName : '';
  }

  selectEntityCountry(country: any) {
    this.entityManagementService.coiRequestObject.property2 = country ? country.countryCode : '';
    this.entityManagementService.entityDashDefaultValues.countrySearch = country ? country.countryName : '';
  }

  onLookupSelect(data: any, property: string) {
    this.lookupValues[property] = data;
    this.entityManagementService.coiRequestObject[property] = data.length ? data.map(d => d.code) : [];
  }


  resetAdvanceSearchFields() {
    this.entityManagementService.coiRequestObject = new EntityDashboardRequest();
    this.entityManagementService.entityDashDefaultValues = new EntityDashDefaultValues();
    this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonService.baseUrl);
    this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
    this.lookupValues = [];
  }

  clearAdvancedSearch() {
    this.resetAdvanceSearchFields();
    this.entityManagementService.coiRequestObject.tabName = this.activeTabName;
    this.viewListOfEntity();
  }
  navigateNextPage(event) {
    this.viewListOfEntity();
  }

  advancedSearch(){
    this.viewListOfEntity();
    this.isSearchData = true;
  }

  updateEntityListDashboard(event){
    if(event) {
      this.viewListOfEntity();
    }
  }

  generateLookupArrayForDropdown() {
		if (this.entityManagementService.coiRequestObject.property20.length !== 0) {
			this.generateLookupArray(this.entityManagementService.coiRequestObject.property20, 'property20');
		}
		if (this.entityManagementService.coiRequestObject.property21.length !== 0) {
			this.generateLookupArray(this.entityManagementService.coiRequestObject.property21, 'property21');
		}
		if (this.entityManagementService.coiRequestObject.property22.length !== 0) {
			this.generateLookupArray(this.entityManagementService.coiRequestObject.property22, 'property22');
		}
	}

	generateLookupArray(property, propertyNumber) {
		this.lookupValues[propertyNumber] = [];
		property.forEach(element => {
			this.lookupValues[propertyNumber].push({ code: element });
		});
	}

}
