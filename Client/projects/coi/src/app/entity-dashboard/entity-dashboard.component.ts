import { Component } from '@angular/core';
import { Router } from '@angular/router';
import { deepCloneObject, isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { GraphDetail } from 'projects/shared/src/lib/graph/interface';
import { Subject, Subscription } from 'rxjs';
import { switchMap } from 'rxjs/operators';
import { HTTP_ERROR_STATUS } from '../app-constants';
import { CommonService } from '../common/services/common.service';
import { ElasticConfigService } from '../common/services/elastic-config.service';
import { NavigationService } from '../common/services/navigation.service';
import { getEndPointOptionsForCountry } from '../configuration/form-builder-create/shared/form-builder-view/search-configurations';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { EntityDashboardRequest, EntityDashboardService, EntityDashDefaultValues, SortCountObj } from './entity-dashboard.service';
import { slideInOut } from 'projects/fibi/src/app/common/utilities/animations';
import { listAnimation, topSlideInOut, fadeInOutHeight, slideInAnimation, scaleOutAnimation } from '../common/utilities/animations';

@Component({
  selector: 'app-entity-dashboard-component',
  templateUrl: './entity-dashboard.component.html',
  styleUrls: ['./entity-dashboard.component.scss'],
  animations: [slideInOut, listAnimation, topSlideInOut, fadeInOutHeight,
    slideInAnimation('0', '12px', 400, 'slideUp'),
    slideInAnimation('0', '-12px', 400, 'slideDown'),
    scaleOutAnimation('-2px', '0', 200, 'scaleOut'),
  ]
})

export class EntityDashboardComponent {
  graphEvent: Subject<GraphDetail> = new Subject<GraphDetail>();
  activeTabName = 'ALL_ENTITIES';
  isViewAdvanceSearch = false;
  $coiList = new Subject();
  coiList = [];
  isCoiEditEntity = false;
  clearField: String;
  advanceSearchDates = { certificationDate: null, expirationDate: null };
  advSearchClearField: String;
  EntitySearchOptions: any = {};
  countrySearchOptions: any = {};
  lookupValues = [];
  entityTypeOptions = 'entity_type#ENTITY_TYPE_CODE#true#true';
  statusTypeOptions = 'EMPTY#EMPTY#true#true#true#true';
  verificationTypeOptions = 'entity_status#ENTITY_STATUS_CODE#true#true';
  $entityList = new Subject();
  entityList: any = [];
  $subscriptions: Subscription[] = [];
  resultCount = 0;
  isShowEntityList = false;
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
    { variableName: 'country', fieldName: 'Country' },
    { variableName: 'updateTimeStamp', fieldName: 'Last Updated' },
];

  constructor(private _router: Router,
    public entityDashboardService: EntityDashboardService,
    private _navigationService: NavigationService,
    private _commonService: CommonService, public sfiService: SfiService,
    private _elasticConfig: ElasticConfigService
) { }

  ngOnInit() {
    this.checkForSort();
    this.EntitySearchOptions = this._elasticConfig.getElasticForEntity();
    this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
    this.isViewAdvanceSearch = false;
    if (!this.entityDashboardService.coiRequestObject.tabName) {
      this.entityDashboardService.coiRequestObject.tabName = this.activeTabName;
    } else {
      this.activeTabName = this.entityDashboardService.coiRequestObject.tabName;
      this.EntitySearchOptions.defaultValue = this.entityDashboardService.entityDashDefaultValues.entitySearch;
      this.countrySearchOptions.defaultValue = this.entityDashboardService.entityDashDefaultValues.countrySearch;
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
    this.entityDashboardService.isShowEntityNavBar = false;
  }

  entityTabName(tabName: string) {
    this.localCOIRequestObject = new EntityDashboardRequest();
    this.resetAdvanceSearchFields();
    this.entityList = [];
    this.resultCount = null;
    this.activeTabName = tabName;
    this.entityDashboardService.coiRequestObject.tabName = this.activeTabName;
    this.isShowAllProposalList = true;
    this.entityDashboardService.isAdvanceSearch = false;
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
    if ( this.entityDashboardService.isAdvanceSearch) {
       this.isViewAdvanceSearch = true;
    }
    if (this.activeTabName === 'ALL_ENTITIES') {
      this.isViewAdvanceSearch = true;
      this.isShowEntityList = false;
      if (this.entityDashboardService.isAdvanceSearch) {
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
        return this.entityDashboardService.getAllSystemEntityList(this.entityDashboardService.coiRequestObject);
      })).subscribe((res: any) => {
        this.entityList = res.entityList || [];
        this.resultCount = res.entityCount;
        this.isLoading = false;
      }, _error => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        this.entityList = [];
        this.isLoading = false;
      }));
  }

  addNewEntity() {
    this.isCoiEditEntity = false;
    this._router.navigate(['/coi/manage-entity/create-entity']);
  }

  selectedEvent(event) {
    this.entityDashboardService.coiRequestObject.property1 = event ? event.entity_name : '';
    this.entityDashboardService.entityDashDefaultValues.entitySearch = event ? event.entity_name : '';
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
    this.entityDashboardService.coiRequestObject.property2 = country ? country.countryCode : '';
    this.entityDashboardService.entityDashDefaultValues.countrySearch = country ? country.countryName : '';
  }

  onLookupSelect(data: any, property: string) {
    this.lookupValues[property] = data;
    this.entityDashboardService.coiRequestObject[property] = data.length ? data.map(d => d.code) : [];
  }

  resetAdvanceSearchFields() {
    this.entityDashboardService.coiRequestObject = new EntityDashboardRequest();
    this.entityDashboardService.entityDashDefaultValues = new EntityDashDefaultValues();
    this.EntitySearchOptions = this._elasticConfig.getElasticForEntity();
    this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
    this.lookupValues = [];
  }

  clearAdvancedSearch() {
    this.resetAdvanceSearchFields();
    this.entityDashboardService.coiRequestObject.tabName = this.activeTabName;
    this.entityList = [];
    this.$entityList.next();
  }

  advancedSearch() {
    this.entityList = [];
    this.$entityList.next();
    this.isShowEntityList = true;
    this.isShowAllProposalList = true;
    this.entityDashboardService.isAdvanceSearch = true;
    }


  generateLookupArrayForDropdown() {
    if (this.entityDashboardService.coiRequestObject.property20.length !== 0) {
      this.generateLookupArray(this.entityDashboardService.coiRequestObject.property20, 'property20');
    }
    if (this.entityDashboardService.coiRequestObject.property21.length !== 0) {
      this.generateLookupArray(this.entityDashboardService.coiRequestObject.property21, 'property21');
    }
    if (this.entityDashboardService.coiRequestObject.property22.length !== 0) {
      this.generateLookupArray(this.entityDashboardService.coiRequestObject.property22, 'property22');
    }
    if (this.entityDashboardService.coiRequestObject.property24.length !== 0) {
        this.generateLookupArray(this.entityDashboardService.coiRequestObject.property24, 'property24');
    }
  }

  generateLookupArray(property, propertyNumber) {
    this.lookupValues[propertyNumber] = [];
    property.forEach(element => {
      this.lookupValues[propertyNumber].push({ code: element });
    });
  }

  viewDetails(entityListData) {
    this._router.navigate(['/coi/manage-entity/entity-overview'], { queryParams: { entityManageId: entityListData.entityId } });
  }

  actionsOnPageChange(event) {
    if (this.entityDashboardService.coiRequestObject.currentPage != event) {
      this.entityDashboardService.coiRequestObject.currentPage = event;
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
        this.entityDashboardService.sortCountObject = deepCloneObject(this.sortCountObj);
        this.entityDashboardService.coiRequestObject.sort = deepCloneObject(this.localCOIRequestObject.sort);
        this.entityDashboardService.sort = deepCloneObject(this.localCOIRequestObject.sort);
        this.$entityList.next();
    }

    checkForSort() {
        if (!isEmptyObject(this.entityDashboardService.coiRequestObject.sort) &&
            this._navigationService.previousURL.includes('manage-entity/create-entity')) {
            this.localCOIRequestObject.sort = deepCloneObject(this.entityDashboardService.coiRequestObject.sort);
            this.sortCountObj = deepCloneObject(this.entityDashboardService.sortCountObject);
        } else {
            this.resetSortObjects();
        }
    }

    resetSortObjects() {
        this.localCOIRequestObject.sort = { 'updateTimeStamp': 'desc' };
        this.entityDashboardService.coiRequestObject.sort = { 'updateTimeStamp': 'desc' };
        this.sortCountObj = new SortCountObj();
        this.entityDashboardService.sortCountObject = new SortCountObj();
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
        this.entityDashboardService.isAdvanceSearch = this.isViewAdvanceSearch;
    }

    openGraph(entityId, entityName) {
        this.graphEvent.next({ visible: true, id: entityId, name: entityName });
    }
}
