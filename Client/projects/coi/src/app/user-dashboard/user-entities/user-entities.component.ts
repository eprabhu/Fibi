import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { SFIDashboardRequestObject } from "../../disclosure/coi-interface";
import { CoiService } from "../../disclosure/services/coi.service";
import { UserEntitiesService } from "./user-entities.service";

@Component({
  selector: 'app-user-entities',
  templateUrl: './user-entities.component.html',
  styleUrls: ['./user-entities.component.scss'],
  providers: [UserEntitiesService]
})
export class UserEntitiesComponent implements OnInit {
  currentSelected = {
    tab: 'IN_PROGRESS',
    filter: 'ALL'
  }
  sfiDashboardRequestObject = new SFIDashboardRequestObject();
  $subscriptions = [];
  entityArray = [];
  filteredEntityArray = [];
  searchText = '';

  constructor(private _userEntityService: UserEntitiesService, private _router: Router) {
  }

  ngOnInit(): void {
    this.$subscriptions.push(this._userEntityService.getSFIDashboard(this.sfiDashboardRequestObject).subscribe((data: any) => {
      this.entityArray = data.coiFinancialEntityList;
      this.setFilter();
    }));
  }

  viewEntityDetails(entities) {
    this._router.navigate(['/coi/entity-details'], { queryParams: { entityId: entities.coiFinancialEntityId,mode:'edit' } })
  }

  setFilter(type = 'ALL') {
    this.currentSelected.filter = type;
    this.filterDashboardData();
  }

  filterDashboardData() {
    if (this.currentSelected.filter == 'ALL') {
      this.filteredEntityArray = this.entityArray;
    } else {
      this.filteredEntityArray = this.entityArray.filter(entity => {
        return this.currentSelected.filter == 'Active' ? entity.isActive == 'Y':entity.isActive == 'N'
      });
    }
  }

}
