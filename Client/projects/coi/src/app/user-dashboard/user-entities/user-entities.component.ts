import { Component, OnDestroy, OnInit } from '@angular/core';
import { NavigationEnd, Router } from '@angular/router';
import { SFIDashboardRequestObject } from "../../disclosure/coi-interface";
import { CoiService } from "../../disclosure/services/coi.service";
import { SfiService } from '../../disclosure/sfi/sfi.service';
import { UserEntitiesService } from "./user-entities.service";
import { CommonService } from '../../common/services/common.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';

@Component({
  selector: 'app-user-entities',
  templateUrl: './user-entities.component.html',
  styleUrls: ['./user-entities.component.scss'],
  providers: [UserEntitiesService]
})
export class UserEntitiesComponent implements OnInit, OnDestroy {
  currentSelected = {
    tab: 'IN_PROGRESS',
    filter: 'ALL'
  }
  sfiDashboardRequestObject = new SFIDashboardRequestObject();
  $subscriptions = [];
  entityArray = [];
  filteredEntityArray = [];
  searchText = '';
  result: any;

  constructor(private _userEntityService: UserEntitiesService, private _router: Router,
    private _sfiService: SfiService, private _commonServices: CommonService) {
  }

  ngOnInit(): void {
    this.fetchMyEntities();
  }
  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  private fetchMyEntities() {
    this.$subscriptions.push(this._userEntityService.getSFIDashboard(this.sfiDashboardRequestObject).subscribe((data: any) => {
      this.result = data;
      this.filteredEntityArray = data.coiFinancialEntityList || [];
    }));
  }

  viewEntityDetails(entities) {
    this._router.navigate(['/coi/entity-details'], { queryParams: { entityId: entities.coiFinancialEntityId, mode: 'view' } })
  }

  setFilter(type = 'ALL') {
    this.currentSelected.filter = type;
    this.sfiDashboardRequestObject.filterType = type;
    this.sfiDashboardRequestObject.currentPage = '1';
    this.fetchMyEntities();
  }

  listenForAdd() {
    this.$subscriptions.push(
        this._sfiService.$addSfi.subscribe((data: boolean) => {
            this.fetchMyEntities();
            this._sfiService.isShowSfiNavBar = false;
            this.removeEntityId();
        })
    );
}

removeEntityId() {
    this._router.navigate([], {
      queryParams: {entityId: null},
      queryParamsHandling: 'merge'
    })
  }


  getRelationshipTypes(relationshipTypes) {
    if(relationshipTypes) {
      return relationshipTypes.split(',').map((type: any) => {
        const lowercase = type.toLowerCase();
        return ' ' + lowercase.charAt(0).toUpperCase() + lowercase.slice(1);
      }).join(',');
    }
  }

  actionsOnPageChangeEvent(event) {
    this.sfiDashboardRequestObject.currentPage = event;
    this.fetchMyEntities();
  }


}
