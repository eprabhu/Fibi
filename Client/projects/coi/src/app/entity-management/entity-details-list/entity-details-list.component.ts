import { Component, OnDestroy, OnInit, SimpleChanges } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { EntityManagementService, RelationshipDashboardRequest } from '../entity-management.service';
import { slowSlideInOut } from '../../../../../fibi/src/app/common/utilities/animations';
import { Subject, Subscription } from 'rxjs';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { ElasticConfigService } from '../../../../../fibi/src/app/common/services/elastic-config.service';
import { getEndPointOptionsForLeadUnit } from '../../../../../fibi/src/app/common/services/end-point.config';
import { parseDateWithoutTimestamp } from '../../../../../fibi/src/app/common/utilities/date-utilities';
import { switchMap } from 'rxjs/operators';
import { CREATE_DISCLOSURE_ROUTE_URL, HTTP_ERROR_STATUS, POST_CREATE_DISCLOSURE_ROUTE_URL } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';
import { DATE_PLACEHOLDER } from '../../../../../fibi/src/app/app-constants';
import { isEmptyObject } from '../../../../../fibi/src/app/common/utilities/custom-utilities';

@Component({
  selector: 'app-entity-details-list',
  templateUrl: './entity-details-list.component.html',
  styleUrls: ['./entity-details-list.component.scss'],
  animations: [slowSlideInOut]
})
export class EntityDetailsListComponent implements OnInit, OnDestroy {

  entityDetails: any = [];
  isviewDetails: true;
  currentSelected = 'PERSON';
  entityManageId = null;
  coiElastic = null;
  datePlaceHolder = DATE_PLACEHOLDER;
  isViewAdvanceSearch = false;
  $subscriptions: Subscription[] = [];
  advSearchClearField: String;
  elasticPersonSearchOptions: any = {};
  leadUnitSearchOptions: any = {};
  lookupValues = [];
  statusTypeOptions = 'EMPTY#EMPTY#true#true';
  advanceSearchDates = {
    startDate: null,
    endDate: null
  };
  resultCount = 0;
  $relationshipEntityList = new Subject();
  isViewPersonalEntities = false;
  rightsArray: any = [];
  sortMap: any = {};
  sortCountObj: any = {};
  isHover: [] = [];
  isViewProjectDisclosure = false;
  isViewTravelDisclosure = false;
  isViewFcoiDisclosure = false;

  constructor(private _router: Router, private _route: ActivatedRoute, public entityManagementService: EntityManagementService,
    private _elasticConfig: ElasticConfigService, public commonService: CommonService) { }


  ngOnInit() {
    this.entityManagementService.relationshipDashboardRequest.filterType = 'PERSON';
    this.getPermissions();
    this.getRelationshipEntityList();
    this.elasticPersonSearchOptions = this._elasticConfig.getElasticForPerson();
    this.leadUnitSearchOptions = getEndPointOptionsForLeadUnit();
    this.$relationshipEntityList.next();
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  viewDetails(data) {
    this._router.navigate(['/coi/entity-details'], { queryParams: { entityId: data.personEntityId, mode: 'view' } });
  }

  redirectToEntity(event) {
    this._router.navigate(['/coi/entity-details'], { queryParams: { entityId: event.personEntityId, mode: 'view' } });
  }

  getRelationshipEntityList() {
    const id = parseInt(this._route.snapshot.queryParamMap.get('entityManageId'));
    this.entityManagementService.relationshipDashboardRequest.id = id;
    this.$subscriptions.push(
      this.$relationshipEntityList.pipe(
      switchMap(() =>
      this.entityManagementService.getPersonEntityDashboard(this.entityManagementService.relationshipDashboardRequest)
        ))
        .subscribe((res: any) => {
          this.entityDetails = res.data || [];
          this.resultCount = res.count;
        }, error => {
          this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
  }

  currentTab(tab) {
    this.resetAdvanceSearchFields();
    this.entityManagementService.relationshipDashboardRequest.filterType = tab;
    this.entityManagementService.relationshipDashboardRequest.id =  parseInt(this._route.snapshot.queryParamMap.get('entityManageId'));
    this.entityDetails = [];
    this.getPermissions();
    // this.getRelationshipEntityList();
    this.$relationshipEntityList.next();
  }


  selectPersonName(person: any) {
    this.entityManagementService.relationshipDashboardRequest.property3 = person ? person.prncpl_id : null;
  }

  leadUnitChangeFunction(unit: any) {
    this.entityManagementService.relationshipDashboardRequest.property2 = unit ? unit.unitNumber : null;
  }

  onLookupSelect(data: any, property: string) {
    this.lookupValues[property] = data;
    this.entityManagementService.relationshipDashboardRequest[property] = data.length ? data.map(d => d.code) : [];
  }

  advanceSearchRelationships() {
    this.entityManagementService.relationshipDashboardRequest.property1 = parseDateWithoutTimestamp(this.advanceSearchDates.startDate);
    this.entityManagementService.relationshipDashboardRequest.property2 = parseDateWithoutTimestamp(this.advanceSearchDates.endDate);
    // this.getRelationshipEntityList();
    this.$relationshipEntityList.next();

  }

  actionsOnPageChange(event) {
    this.entityManagementService.relationshipDashboardRequest.currentPage = event;
    this.$relationshipEntityList.next();
  }

  resetAdvanceSearchFields() {
    this.advanceSearchDates.endDate = null;
    this.advanceSearchDates.startDate = null;
    this.entityManagementService.relationshipDashboardRequest = new RelationshipDashboardRequest();
    this.advSearchClearField = new String('true');
    this.lookupValues = [];
  }

  clearAdvanceSearch() {
    this.resetAdvanceSearchFields();
    // this.getRelationshipEntityList();
    this.$relationshipEntityList.next();
  }

   getPermissions() {
    this.isViewPersonalEntities = this.getAvailableRight('VIEW_ENTITY') &&
      this.entityManagementService.relationshipDashboardRequest.filterType === 'PERSON' ||
      this.entityManagementService.relationshipDashboardRequest.filterType === 'OTHER_ENTITIES';
    this.isViewProjectDisclosure = this.getAvailableRight(['VIEW_PROJECT_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE']) &&
      this.entityManagementService.relationshipDashboardRequest.filterType === 'DEPARTMENT';
    this.isViewTravelDisclosure = this.getAvailableRight(['VIEW_TRAVEL_DISCLOSURE', 'MANAGE_TRAVEL_DISCLOSURE']) &&
      this.entityManagementService.relationshipDashboardRequest.filterType === 'TRAVEL_DISCLOSURES';
    this.isViewFcoiDisclosure = this.getAvailableRight(['VIEW_FCOI_DISCLOSURE', 'MANAGE_FCOI_DISCLOSURE']) &&
      this.entityManagementService.relationshipDashboardRequest.filterType === 'FINANCIAL_DISCLOSURES';
  }

   getAvailableRight(rights): boolean {
    const isRightAvailable = Array.isArray(rights) ?
      rights.some((right) => this.commonService.rightsArray.includes(right)) : this.commonService.rightsArray.includes(rights);
    return isRightAvailable;
  }
  /* this commented method need for list sorting.

  // sortResult(sortFieldBy) {
  //   this.sortCountObj[sortFieldBy]++;
  //   if (this.sortCountObj[sortFieldBy] < 3) {
  //     this.sortMap[sortFieldBy] = !this.sortMap[sortFieldBy] ? 'asc' : 'desc';
  //   } else {
  //     this.sortCountObj[sortFieldBy] = 0;
  //     delete this.sortMap[sortFieldBy];
  //   }
  //   this.entityManagementService.relationshipDashboardRequest.sort = this.sortMap;
  //   this.$relationshipEntityList.next();
  // }

  // isActive(colName) {
  //   if (!isEmptyObject(this.entityManagementService.relationshipDashboardRequest.sort) &&
  //     Object.keys(this.entityManagementService.relationshipDashboardRequest.sort).includes(colName)) {
  //     return true;
  //   } else {
  //     return false;
  //   }
  // }

  */
  redirectToDisclosure(disclosure: any) {
    if (disclosure.reviewStatusCode) {
      const redirectUrl = disclosure.reviewStatusCode === '1' ?
          CREATE_DISCLOSURE_ROUTE_URL : POST_CREATE_DISCLOSURE_ROUTE_URL;
      this._router.navigate([redirectUrl],
          { queryParams: { disclosureId: disclosure.coiDisclosureId } });
    }
}

convertDisclosureStatus(status): string {
  if (status) {
    const lowerCase = status.toLowerCase();
    return  lowerCase.charAt(0).toUpperCase() + lowerCase.slice(1);
  }
}
}
