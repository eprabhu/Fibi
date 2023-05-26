import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { EntityDashDefaultValues, EntityDashboardRequest, EntityManagementService } from '../entity-management.service';
import { Subscription } from 'rxjs';
import { ElasticConfigService } from '../../../../../fibi/src/app/common/services/elastic-config.service';
import { CommonService } from '../../common/services/common.service';
import { slideInOut } from '../../../../../fibi/src/app/common/utilities/animations';
import { getEndPointOptionsForCountry, getEndPointOptionsForEntity } from '../../../../../fibi/src/app/common/services/end-point.config';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { SfiService } from '../../disclosure/sfi/sfi.service';

@Component({
  selector: 'app-entity-list',
  templateUrl: './entity-list.component.html',
  styleUrls: ['./entity-list.component.scss'],
  animations: [slideInOut]
})
export class EntityListComponent implements OnDestroy, OnInit {

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
  resultCount = 0;
  isSearchData = false;
  showEntityList = false;
  isShowAllProposalList = false;
  rightList: string;
  isManageEntity: boolean;

  constructor(private _router: Router,
    public entityManagementService: EntityManagementService,
    private _elasticConfig: ElasticConfigService,
    private _commonService: CommonService, public sfiService: SfiService) { }

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
    this.checkUserHasRight();
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
    this.entityManagementService.isShowEntityNavBar = false;
  }

  selectedEntity(event) {
    this.entityManageId = event;
    this.isCoiEditEntity = true;
    this.entityManagementService.isShowEntityNavBar = true;
  }

  entityTabName(tabName: string) {
    this.resetAdvanceSearchFields();
    this.entityList = [];
    this.resultCount = null;
    this.activeTabName = tabName;
    this.entityManagementService.coiRequestObject.tabName = this.activeTabName;
    this.isShowAllProposalList = true;
    if (this.activeTabName === 'ALL_ENTITIES') {
      this.isSearchData = false;
      this.isViewAdvanceSearch = true;
    } else {
      this.isSearchData = true;
      this.viewListOfEntity();
      this.isViewAdvanceSearch = false;
      this.isShowAllProposalList = true;
    }
  }

  redirectToEntity(coi: any) {
    this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: coi.id } });
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
    this.sfiService.isShowSfiNavBar = true;
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

  advancedSearch() {
    this.viewListOfEntity();
    this.isSearchData = true;
    this.isShowAllProposalList = true;
  }

  updateEntityListDashboard(event) {
    if (event) {
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

  addEntityList(event) {
    this.showEntityList = event;
  }

  viewDetails(entityListData) {
    this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: entityListData.entityId } });
  }

  actionsOnPageChange(event) {
    this.entityManagementService.coiRequestObject.currentPage = event;
    this.viewListOfEntity();
  }

  checkUserHasRight(): void {
    this.isManageEntity = this._commonService.hasRight('MANAGE_ENTITY') ||  this._commonService.hasRight('MANAGE_ENTITY');
  }
}
