import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output } from '@angular/core';
import { Router } from '@angular/router';
import { EntityManagementService } from '../entity-management.service';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';


@Component({
  selector: 'app-entity-list',
  templateUrl: './entity-list.component.html',
  styleUrls: ['./entity-list.component.scss']
})
export class EntityListComponent implements  OnChanges {

  @Output() modifyEntityId: EventEmitter<any> = new EventEmitter<any>();
  @Input() entityActiveTabName: string;
  @Input() entityList: any = [];
  @Output() navNextPage: EventEmitter<any> = new EventEmitter<any>()
  isViewEntityDetails = false;
  $subscriptions: Subscription[] = [];
  @Input() resultCount: number = 0;


  constructor(private _router: Router, private _entityManagementService: EntityManagementService) { }

  viewDetails(entityListData) {
    this._router.navigate(['/coi/entity-management/entity-list'], { queryParams: { entityManageId: entityListData.entityId } })
    // this.viewEntityDetails.emit(value);
  }

  ngOnChanges() {
  }

  modifyEntityDetails(data) {
    this.modifyEntityId.emit(data.entityId);
  }

  actionsOnPageChange(event){
     this._entityManagementService.coiRequestObject.currentPage = event;
     this.navNextPage.emit(event);
  }
}
