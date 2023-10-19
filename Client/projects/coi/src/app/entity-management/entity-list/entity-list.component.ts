import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { EntityDashDefaultValues, EntityDashboardRequest, EntityManagementService, SortCountObj } from '../entity-management.service';
import { Subject, Subscription } from 'rxjs';
import { ElasticConfigService } from '../../../../../fibi/src/app/common/services/elastic-config.service';
import { CommonService } from '../../common/services/common.service';
import { slideInOut } from '../../../../../fibi/src/app/common/utilities/animations';
import { listAnimation, topSlideInOut, fadeInOutHeight, scaleOutAnimation, slideInAnimation } from '../../common/utilities/animations';
import { getEndPointOptionsForCountry, getEndPointOptionsForEntity } from '../../../../../fibi/src/app/common/services/end-point.config';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { SfiService } from '../../disclosure/sfi/sfi.service';
import { deepCloneObject, isEmptyObject } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { NavigationService } from '../../common/services/navigation.service';
import { switchMap } from 'rxjs/operators';
import { GraphDetail } from '../../../../../shared/src/lib/graph/interface';


@Component({
  selector: 'app-entity-list',
  templateUrl: './entity-list.component.html',
  styleUrls: ['./entity-list.component.scss'],
  animations: [slideInOut, listAnimation, topSlideInOut, fadeInOutHeight,
    slideInAnimation('0', '12px', 400, 'slideUp'),
    slideInAnimation('0', '-12px', 400, 'slideDown'),
    scaleOutAnimation('-2px', '0', 200, 'scaleOut'),
]
})
export class EntityListComponent implements OnDestroy, OnInit {

  graphEvent: Subject<GraphDetail> = new Subject<GraphDetail>();
  activeTabName = 'ALL_ENTITIES';
  isViewAdvanceSearch = false;
  coiElastic = null;
  $coiList = new Subject();
  coiList = [];
  isCoiEditEntity = false;
  clearField: String;
  advanceSearchDates = { certificationDate: null, expirationDate: null };
  advSearchClearField: String;
  EntitySearchOptions: any = {};
  countrySearchOptions: any = {};
  lookupValues = [];
  riskLevelTypeOptions = 'entity_risk_category#RISK_CATEGORY_CODE#true#true';
  entityTypeOptions = 'entity_type#ENTITY_TYPE_CODE#true#true';
  statusTypeOptions = 'EMPTY#EMPTY#true#true#true#true';
  verificationTypeOptions = 'entity_status#ENTITY_STATUS_CODE#true#true';
  $entityList = new Subject();
  entityList: any = [];
  $subscriptions: Subscription[] = [];
  resultCount = 0;
  isShowEntityList = false;
  showEntityList = false;
  isShowAllProposalList = false;
  rightList: string;
  isManageEntity: boolean;
  sortCountObj: SortCountObj;
  localCOIRequestObject: EntityDashboardRequest = new EntityDashboardRequest();
  result: any;
  isActiveDisclosureAvailable: boolean;
  isLoading = false;
  isShowGraph = false;
  sortSectionsList = [
    { variableName: 'name', fieldName: 'Name' },
    { variableName: 'entityType', fieldName: 'Entity Type' },
    { variableName: 'riskLevel', fieldName: 'Risk' },
    { variableName: 'country', fieldName: 'Country' },
    { variableName: 'updateTimeStamp', fieldName: 'Last Updated' },
];
isEntityFound = false;

  constructor(private _router: Router,
    public entityManagementService: EntityManagementService,
    private _elasticConfig: ElasticConfigService,
    private _navigationService: NavigationService,
    private _commonService: CommonService, public sfiService: SfiService) { }

  ngOnInit() {
    this.checkForSort();
    this.coiElastic = this._elasticConfig.getElasticForCoi();
    this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonService.baseUrl, 'ALL');
    this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
    this.isViewAdvanceSearch = false;
    if (!this.entityManagementService.coiRequestObject.tabName) {
      this.entityManagementService.coiRequestObject.tabName = this.activeTabName;
    } else {
      this.activeTabName = this.entityManagementService.coiRequestObject.tabName;
      this.EntitySearchOptions.defaultValue = this.entityManagementService.entityDashDefaultValues.entitySearch;
      this.countrySearchOptions.defaultValue = this.entityManagementService.entityDashDefaultValues.countrySearch;
      this.generateLookupArrayForDropdown();
    }
    this.checkUserHasRight();
    this.viewListOfEntity();
    this.checkForAdvanceSearch();
    this.showEntities();
    this.isShowGraph = this._commonService.enableGraph;
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
    this.entityManagementService.isShowEntityNavBar = false;
  }

  selectedEntity() {
    this.isCoiEditEntity = true;
    this.entityManagementService.isShowEntityNavBar = true;
  }

  entityTabName(tabName: string) {
    this.localCOIRequestObject = new EntityDashboardRequest();
    this.resetAdvanceSearchFields();
    this.entityList = [];
    this.resultCount = null;
    this.activeTabName = tabName;
    this.entityManagementService.coiRequestObject.tabName = this.activeTabName;
    this.isShowAllProposalList = true;
    this.entityManagementService.isAdvanceSearch = false;
    this.isEntityFound = false;
    if (this.activeTabName === 'ALL_ENTITIES') {
        this.isShowEntityList = false;
      this.isViewAdvanceSearch = true;
    } else {
      this.isShowEntityList = true;
      this.$entityList.next();
      this.isViewAdvanceSearch = false;
      this.isShowAllProposalList = true;
    }
  }
  checkForAdvanceSearch() {
    if ( this.entityManagementService.isAdvanceSearch) {
       this.isViewAdvanceSearch = true;
    }
    if (this.activeTabName === 'ALL_ENTITIES') {
      this.isViewAdvanceSearch = true;
      this.isShowEntityList = false;
      if (this.entityManagementService.isAdvanceSearch) {
        this.isShowEntityList = true;
        this.$entityList.next();
      }
    }
  }

  redirectToEntity(coi: any) {
    this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: coi.id } });
  }

 viewListOfEntity() {
    this.$subscriptions.push(this.$entityList.pipe(
      switchMap(() => {
        this.isLoading = true;
        return this.entityManagementService.getAllSystemEntityList(this.entityManagementService.coiRequestObject);
      })).subscribe((res: any) => {
        this.entityList = res.coiEntityList || [];
        this.resultCount = res.entityCount;
        this.isLoading = false;
        if (!this.entityList.length) {
          this.isEntityFound = true;
       }
      }, _error => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        this.entityList = [];
        this.isLoading = false;
      }));
  }

  addNewEntity() {
    this.isCoiEditEntity = false;
    this._router.navigate(['/coi/create-sfi/create'], { queryParams: { type: 'ENTITY' } });
  }

  selectedEvent(event) {
    this.entityManagementService.coiRequestObject.property1 = event ? event.entityName : '';
    this.entityManagementService.entityDashDefaultValues.entitySearch = event ? event.entityName : '';
  }
  showEntities() {
    if (this.activeTabName !== 'ALL_ENTITIES' ) {
      this.$entityList.next();
      this.isShowEntityList = true;
      this.isShowAllProposalList = true;
      if (this.isViewAdvanceSearch) {
       this. isViewAdvanceSearch = true;
      }
    }
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
    this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonService.baseUrl, 'ALL');
    this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
    this.lookupValues = [];
  }

  clearAdvancedSearch() {
    this.resetAdvanceSearchFields();
    this.entityManagementService.coiRequestObject.tabName = this.activeTabName;
    this.entityList = [];
    this.$entityList.next();
  }
  navigateNextPage(event) {
    this.$entityList.next();
  }

  advancedSearch() {
    this.entityList = [];
    this.$entityList.next();
    this.isShowEntityList = true;
    this.isShowAllProposalList = true;
    this.entityManagementService.isAdvanceSearch = true;
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
    if (this.entityManagementService.coiRequestObject.property24.length !== 0) {
        this.generateLookupArray(this.entityManagementService.coiRequestObject.property24, 'property24');
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
    if (this.entityManagementService.coiRequestObject.currentPage != event) {
      this.entityManagementService.coiRequestObject.currentPage = event;
      this.$entityList.next();
    }
  }

  checkUserHasRight(): void {
    this.isManageEntity = this._commonService.getAvailableRight(['MANAGE_ENTITY', 'VIEW_ENTITY'], 'SOME');
  }

setEventTypeFlag() {
  this.isActiveDisclosureAvailable = !!this.coiList.find((ele: any) => ele.disclosureSequenceStatusCode === '2');
}

    sortResult(sortFieldBy) {
        this.sortCountObj[sortFieldBy]++;
        if (this.sortCountObj[sortFieldBy] < 3) {
            this.localCOIRequestObject.sort[sortFieldBy] = !this.localCOIRequestObject.sort[sortFieldBy] ? 'asc' : 'desc';
        } else {
            this.sortCountObj[sortFieldBy] = 0;
            delete this.localCOIRequestObject.sort[sortFieldBy];
        }
        this.entityManagementService.sortCountObject = deepCloneObject(this.sortCountObj);
        this.entityManagementService.coiRequestObject.sort = deepCloneObject(this.localCOIRequestObject.sort);
        this.entityManagementService.sort = deepCloneObject(this.localCOIRequestObject.sort);
        this.$entityList.next();
    }

    checkForSort() {
        if (!isEmptyObject(this.entityManagementService.coiRequestObject.sort) &&
            this._navigationService.previousURL.includes('entity-management/entity-details')) {
            this.localCOIRequestObject.sort = deepCloneObject(this.entityManagementService.coiRequestObject.sort);
            this.sortCountObj = deepCloneObject(this.entityManagementService.sortCountObject);
        } else {
            this.resetSortObjects();
        }
    }

    resetSortObjects() {
        this.localCOIRequestObject.sort = { 'updateTimeStamp': 'desc' };
        this.entityManagementService.coiRequestObject.sort = { 'updateTimeStamp': 'desc' };
        this.sortCountObj = new SortCountObj();
        this.entityManagementService.sortCountObject = new SortCountObj();
    }

    isActive(colName) {
        if (!isEmptyObject(this.localCOIRequestObject.sort) && colName in this.localCOIRequestObject.sort) {
            return true;
        } else {
            return false;
        }

    }

    toggleAdvanceSearch() {
        this.isViewAdvanceSearch = !this.isViewAdvanceSearch;
        this.entityManagementService.isAdvanceSearch = this.isViewAdvanceSearch;
    }

    openGraph(entityId, entityName) {
        this.graphEvent.next({ visible: true, id: entityId, name: entityName });
    }

}
