import { Component, OnInit } from '@angular/core';
import { parseDateWithoutTimestamp } from '../../../../fibi/src/app/common/utilities/date-utilities';
import { slowSlideInOut, slideHorizontal, fadeDown, slideInOut } from '../../../../fibi/src/app/common/utilities/animations';
import { EntityManagementService } from './entity-management.service';

@Component({
  selector: 'app-entity-management',
  templateUrl: './entity-management.component.html',
  styleUrls: ['./entity-management.component.scss'],
  animations: [slideInOut, slowSlideInOut, fadeDown]
})
export class EntityManagementComponent implements OnInit {

  viewDetails = false;
  activeTabName = 'ALL_ENTITIES';
  isViewAdvanceSearch = false;
  isHasSfiOn = true;
  isHasDisclosureOn = true;

  constructor(private _entityManagementService: EntityManagementService) {
  }
  ngOnInit() {
    this.getCOIAdminDashboard()
  }

  selectedEntity(event) {
    this.viewDetails = event;
  }

  hideEntityDetails(event) {
    this.viewDetails = event;
  }

  entityTabName(tabName) {
    this.activeTabName = tabName
  }

  getCOIAdminDashboard() {

    this._entityManagementService.getCOIAdminDashboard(this._entityManagementService.coiRequestObject).subscribe((res: any) => {
      // console.log('res',res);

    });
  }


  getRequestObject() {
    this.setAdvanceSearchValuesToServiceObject();
    return this._entityManagementService.coiRequestObject;
  }
  setAdvanceSearchValuesToServiceObject() {
    // this._entityManagementService.coiRequestObject.property6 = parseDateWithoutTimestamp(this.advanceSearchDates.startDate);
    // this._entityManagementService.coiRequestObject.property7 = parseDateWithoutTimestamp(this.advanceSearchDates.endDate);
    // this._entityManagementService.coiRequestObject.property15 =
    //     this._entityManagementService.coiRequestObject.advancedSearch === 'L'
    //         ? null : this._entityManagementService.coiRequestObject.property15;
  }
}
