import { Component, EventEmitter, OnInit, Output } from '@angular/core';
import { Router } from '@angular/router';
import { EntityManagementService } from '../entity-management.service';

@Component({
  selector: 'app-entity-list',
  templateUrl: './entity-list.component.html',
  styleUrls: ['./entity-list.component.scss']
})
export class EntityListComponent implements OnInit {

  @Output() viewEntityDetails: EventEmitter<any> = new EventEmitter<any>();

  isViewEntityDetails = false;
  entityList = [];
  constructor(private _router: Router, private _entityManagementService: EntityManagementService) { }

  ngOnInit() {

    this.viewListOfEntity();
  }

  viewDetails(entityListData) {
     this._router.navigate(['/coi/entity-management/entity-list'], { queryParams: { entityManageId: entityListData.id } })
    // this.viewEntityDetails.emit(value);
  }

  viewListOfEntity() {
    this._entityManagementService.getAllSystemEntityList().subscribe((res:any) => {
      // this.entityList = res;
      console.log(res)
      debugger
    })

    this.entityList = [
      {'id':'1','verified':'V','name': 'Alexion Pharmaceuticals', 'country': 'US', 'entityType': 'For Profit', 'riskLevel': 'High', 'status': 'Active' },
      {'id':'2', 'verified':'V','name': 'Bausch health companies', 'country': 'Canada', 'entityType': 'Non Profit', 'riskLevel': 'Medium', 'status': 'Inactive' },
      {'id':'3', 'verified':'U','name': 'Daichi Sankyo Co., Ltd.', 'country': 'Japan', 'entityType': 'Non Profit', 'riskLevel': 'Low', 'status': 'Inactive' }
    ];


  }
}
