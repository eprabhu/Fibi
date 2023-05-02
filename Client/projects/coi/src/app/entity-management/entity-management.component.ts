import { Component, OnDestroy, OnInit } from '@angular/core';
import { parseDateWithoutTimestamp } from '../../../../fibi/src/app/common/utilities/date-utilities';
import { slowSlideInOut, slideHorizontal, fadeDown, slideInOut } from '../../../../fibi/src/app/common/utilities/animations';
import { EntityDashboardRequest, EntityManagementService } from './entity-management.service';
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
  animations: [slideInOut, slowSlideInOut, fadeDown]
})
export class EntityManagementComponent implements OnInit, OnDestroy {

  entityManageId = null;
  activeTabName = 'ALL_ENTITIES';
  isViewAdvanceSearch = false;
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
  $entityDetailsList = new Subject();



  constructor(public entityManagementService: EntityManagementService,
    private _elasticConfig: ElasticConfigService, private _router: Router,
    private _commonService:CommonService) {
  }
  ngOnInit() {
    this.coiElastic = this._elasticConfig.getElasticForCoi();
    this.EntitySearchOptions = getEndPointOptionsForEntity();
    this.countrySearchOptions = getEndPointOptionsForCountry();
    this.entityManagementService.coiRequestObject.tabName = this.activeTabName;
    this.viewListOfEntity();
    this.$entityDetailsList.next();

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
    this.$entityDetailsList.next()
  }

  redirectToEntity(coi: any) {
    this._router.navigate(['/coi/entity-management/entity-list'], { queryParams: { entityManageId: coi.id } });
  }

  viewListOfEntity() {
    this.$subscriptions.push(
      this.$entityDetailsList.pipe(
        switchMap(() =>
          this.entityManagementService.getAllSystemEntityList(this.entityManagementService.coiRequestObject)
        ))
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
  }

  selectEntityCountry(country: any) {
    this.entityManagementService.coiRequestObject.property2 = country ? country.countryCode : '';
  }

  onLookupSelect(data: any, property: string) {
    this.lookupValues[property] = data;
    this.entityManagementService.coiRequestObject[property] = data.length ? data.map(d => d.code) : [];
  }


  resetAdvanceSearchFields() {
    this.entityManagementService.coiRequestObject = new EntityDashboardRequest();
    this.advSearchClearField = new String('true');
    this.clearField = new String('true');
    this.lookupValues = [];
  }

  clearAdvancedSearch() {
    this.resetAdvanceSearchFields();
    this.entityManagementService.coiRequestObject.tabName = this.activeTabName;
    this.$entityDetailsList.next();
  }
  navigateNextPage(event) {
    this.$entityDetailsList.next();
  }

  advancedSearch(){
    this.$entityDetailsList.next();
  }

  updateEntityListDashboard(event){
    if(event) {
      this.$entityDetailsList.next();
    }
  }
}
