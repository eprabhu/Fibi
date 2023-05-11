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
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';
import { DATE_PLACEHOLDER } from '../../../../../fibi/src/app/app-constants';

@Component({
  selector: 'app-entity-details-list',
  templateUrl: './entity-details-list.component.html',
  styleUrls: ['./entity-details-list.component.scss'],
  animations: [slowSlideInOut]
})
export class EntityDetailsListComponent implements OnInit,OnDestroy {

  entityDetails: any = [];
  isViewEntityDetails: true;
  isviewDetails: true;
  currentSelected = 'Person';
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
  }
  resultCount: number = 0;
  $relationshipEntityList = new Subject();



  constructor(private _router: Router, private _route: ActivatedRoute, public entityManagementService: EntityManagementService,
    private _elasticConfig: ElasticConfigService,private _commonService:CommonService) { }


  ngOnInit() {
    this.entityManagementService.relationshipDashboardRequest.filterType = 'Person';
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
      switchMap(()=>
      this.entityManagementService.getPersonEntityDashboard(this.entityManagementService.relationshipDashboardRequest)
        ))
        .subscribe((res: any) => {
          this.entityDetails = res.data || [];
          this.resultCount = res.count;
        },_error=>{
          this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
  }

  currentTab(tab) {
    this.resetAdvanceSearchFields();
    this.entityManagementService.relationshipDashboardRequest.filterType = tab;
    // this.getRelationshipEntityList();
    // this.$relationshipEntityList.next();
  }


  selectPersonName(person: any) {
    this.entityManagementService.relationshipDashboardRequest.property3 = person ? person.prncpl_id : null;
  }

  leadUnitChangeFunction(unit: any) {
    // this.entityManagementService.relationshipDashboardRequest.property2 = unit ? unit.unitNumber : null;
  }

  onLookupSelect(data: any, property: string) {
    this.lookupValues[property] = data;
    this.entityManagementService.relationshipDashboardRequest[property] = data.length ? data.map(d => d.code) : [];
  }

  advanceSearchRelationships() {
    this.entityManagementService.relationshipDashboardRequest.property1 = parseDateWithoutTimestamp(this.advanceSearchDates.startDate);
    this.entityManagementService.relationshipDashboardRequest.property2 = parseDateWithoutTimestamp(this.advanceSearchDates.endDate);
    // this.getRelationshipEntityList();
    // this.$relationshipEntityList.next();

  }

  actionsOnPageChange(event) {
    this.entityManagementService.relationshipDashboardRequest.currentPage = event;
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
    // this.$relationshipEntityList.next();

  }
}
