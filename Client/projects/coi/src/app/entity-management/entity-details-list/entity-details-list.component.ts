import { Component, OnChanges, OnDestroy, OnInit, SimpleChanges } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { EntityManagementService } from '../entity-management.service';
import { slowSlideInOut } from '../../../../../fibi/src/app/common/utilities/animations';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { ElasticConfigService } from '../../../../../fibi/src/app/common/services/elastic-config.service';
import { getEndPointOptionsForLeadUnit } from '../../../../../fibi/src/app/common/services/end-point.config';
import { parseDateWithoutTimestamp } from 'projects/fibi/src/app/common/utilities/date-utilities';

@Component({
  selector: 'app-entity-details-list',
  templateUrl: './entity-details-list.component.html',
  styleUrls: ['./entity-details-list.component.scss'],
  animations: [slowSlideInOut]
})
export class EntityDetailsListComponent implements OnInit,OnChanges, OnDestroy {

  entityDetails:any;
  isViewEntityDetails: true;
  isviewDetails: true;
  currentSelected = 'Person';
  entityManageId = null;
  coiElastic = null;
  isViewAdvanceSearch = false;
  $subscriptions: Subscription[] = [];
  advSearchClearField: String;
  elasticPersonSearchOptions: any = {};
  leadUnitSearchOptions: any = {};
  lookupValues = [];
  statusTypeOptions = 'ENTITY_STATUS#ENTITY_STATUS_CODE#true#true';
  advanceSearchDates = {
    startDate:null,
    endDate: null
  }

  constructor(private _router: Router, private _route: ActivatedRoute, public entityManagementService: EntityManagementService,
    private _elasticConfig: ElasticConfigService) { }


  ngOnInit() {
    this.entityManageId = this._route.snapshot.queryParamMap.get('entityManageId');
    this.getRelationshipEntityList();
    this.elasticPersonSearchOptions = this._elasticConfig.getElasticForPerson();
    this.leadUnitSearchOptions = getEndPointOptionsForLeadUnit();


  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  getEntityDetails() {
    this.entityManagementService
  }

  viewDetails(data) {
    this._router.navigate(['/coi/entity-details'], { queryParams: { entityId: '104', mode: 'edit' } });
  }

  redirectToEntity(event) {
    this._router.navigate(['/coi/entity-details'], { queryParams: { entityId: '104' } });
  }

  ngOnChanges() {

  }

  getRelationshipEntityList() {
    const REQ_BODY = {
      'filterType': this.currentSelected,
      'coiEntityId': 1
    }
    this.$subscriptions.push(this.entityManagementService.getPersonEntityDetails(REQ_BODY).subscribe((res: any) => {
      this.entityDetails = res.personEntityList
      console.log(this.entityDetails);
    }));
  }

  currentTab(tab) {
    this.currentSelected = tab;
    this.getRelationshipEntityList();
  }

  selectPersonName(person: any) {
    this.entityManagementService.relationshipDashboardRequest.property1 = person ? person.prncpl_id : null;
  }

  leadUnitChangeFunction(unit: any) {
    this.entityManagementService.relationshipDashboardRequest.property2 = unit ? unit.unitNumber : null;
  }

  onLookupSelect(data: any, property: string) {
    this.lookupValues[property] = data;
    this.entityManagementService.relationshipDashboardRequest[property] = data.length ? data.map(d => d.code) : [];
  }

  advanceSearchRelationships() {
    this.entityManagementService.relationshipDashboardRequest.property4 = parseDateWithoutTimestamp(this.advanceSearchDates.startDate);
    this.entityManagementService.relationshipDashboardRequest.property5 = parseDateWithoutTimestamp(this.advanceSearchDates.endDate);
  }
}
