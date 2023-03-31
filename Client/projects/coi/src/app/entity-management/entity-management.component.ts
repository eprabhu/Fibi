import { Component, OnInit } from '@angular/core';
import { slowSlideInOut,slideHorizontal,fadeDown,slideInOut } from '../../../../fibi/src/app/common/utilities/animations';

@Component({
  selector: 'app-entity-management',
  templateUrl: './entity-management.component.html',
  styleUrls: ['./entity-management.component.scss'],
  animations:[slideInOut,slowSlideInOut,fadeDown]
})
export class EntityManagementComponent implements OnInit {

  viewDetails = false;
  activeTabName = 'ALL_ENTITIES';
  isViewAdvanceSearch = false;
  isHasSfiOn = true;
  isHasDisclosureOn = true;
  constructor() {
  }
  ngOnInit() {
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
}
