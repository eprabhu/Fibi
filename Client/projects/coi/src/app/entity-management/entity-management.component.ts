import { Component, OnDestroy, OnInit } from '@angular/core';
import { parseDateWithoutTimestamp } from '../../../../fibi/src/app/common/utilities/date-utilities';
import { slowSlideInOut, slideHorizontal, fadeDown, slideInOut } from '../../../../fibi/src/app/common/utilities/animations';
import { EntityManagementService } from './entity-management.service';
import { ElasticConfigService } from '../../../../fibi/src/app/common/services/elastic-config.service';
import { Router } from '@angular/router';
import { getEndPointOptionsForCountry, getEndPointOptionsForEntity } from '../../../../fibi/src/app/common/services/end-point.config';

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
  isHasSfiOn = false;
  isHasDisclosureOn = false;
  coiElastic = null;
  isCoiEditEntity = false;
  clearField: String;
  advSearchClearField: String;
  EntitySearchOptions: any = {};
  countrySearchOptions: any = {};
  lookupValues = [];
  riskLevelTypeOptions = 'entity_risk_category#RISK_CATEGORY_CODE#true#true';
  entityTypeOptions = 'entity_type#ENTITY_TYPE_CODE#true#true';
  statusTypeOptions = 'entity_status#ENTITY_STATUS_CODE#true';



  constructor(public entityManagementService: EntityManagementService,
    private _elasticConfig: ElasticConfigService, private _router: Router) {
  }
  ngOnInit() {
    this.coiElastic = this._elasticConfig.getElasticForCoi();
    this.EntitySearchOptions = getEndPointOptionsForEntity();
    this.countrySearchOptions = getEndPointOptionsForCountry();
    this.entityManagementService.coiRequestObject.tabName = this.activeTabName;

  }
  ngOnDestroy() {
    this.entityManagementService.isShowEntityNavBar = false;

  }

  selectedEntity(event) {
    this.entityManageId = event;
    this.isCoiEditEntity = true;
    this.entityManagementService.isShowEntityNavBar = true;
  }

  entityTabName(tabName) {
    this.activeTabName = tabName;
    this.entityManagementService.coiRequestObject.tabName = this.activeTabName;
  }

  getRequestObject() {
    this.setAdvanceSearchValuesToServiceObject();
    return this.entityManagementService.coiRequestObject;
  }
  setAdvanceSearchValuesToServiceObject() {
    // this.entityManagementService.coiRequestObject.property6 = parseDateWithoutTimestamp(this.advanceSearchDates.startDate);
    // this.entityManagementService.coiRequestObject.property7 = parseDateWithoutTimestamp(this.advanceSearchDates.endDate);
    // this.entityManagementService.coiRequestObject.property15 =
    //     this.entityManagementService.coiRequestObject.advancedSearch === 'L'
    //         ? null : this.entityManagementService.coiRequestObject.property15;
  }
  redirectToEntity(coi: any) {
    this._router.navigate(['/coi/entity-management/entity-list'], { queryParams: { entityManageId: coi.id } });
  }

  addNewEntity() {
    this.entityManageId = null;
    this.isCoiEditEntity = false
    this.entityManagementService.isShowEntityNavBar = true;
  }

  selectedEvent(event) {
    this.entityManagementService.coiRequestObject.property1 = event ? event.coiEntityId : null;
  }

  selectEntityCountry(country: any) {
    this.entityManagementService.coiRequestObject.property2 = country ? country.countryCode : null;
  }

  onLookupSelect(data: any, property: string) {
    this.lookupValues[property] = data;
    this.entityManagementService.coiRequestObject[property] = data.length ? data.map(d => d.code) : [];
  }
  onHasSif(data: any) {
    this.entityManagementService.coiRequestObject.property5 = data;
  }
  onHasDisclosure(data: any) {
    this.entityManagementService.coiRequestObject.property6 = data;
  }

  clearAdvancedSearch() {

  }
}
