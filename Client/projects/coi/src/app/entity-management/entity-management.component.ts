import { Component, OnDestroy, OnInit } from '@angular/core';
import { parseDateWithoutTimestamp } from '../../../../fibi/src/app/common/utilities/date-utilities';
import { slowSlideInOut, slideHorizontal, fadeDown, slideInOut } from '../../../../fibi/src/app/common/utilities/animations';
import { EntityManagementService } from './entity-management.service';
import { ElasticConfigService } from '../../../../fibi/src/app/common/services/elastic-config.service';
import { Router } from '@angular/router';

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
  isHasSfiOn = true;
  isHasDisclosureOn = true;
  coiElastic = null;
  isCoiEditEntity = false;

  constructor(public entityManagementService: EntityManagementService,
    private _elasticConfig: ElasticConfigService, private _router: Router) {
  }
  ngOnInit() {
    this.coiElastic = this._elasticConfig.getElasticForCoi();

    // this.getCOIAdminDashboard()
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
    this.activeTabName = tabName
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

}
