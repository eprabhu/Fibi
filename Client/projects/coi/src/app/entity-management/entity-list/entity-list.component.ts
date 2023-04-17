import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output } from '@angular/core';
import { Router } from '@angular/router';
import { EntityManagementService } from '../entity-management.service';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';

@Component({
  selector: 'app-entity-list',
  templateUrl: './entity-list.component.html',
  styleUrls: ['./entity-list.component.scss']
})
export class EntityListComponent implements OnInit, OnChanges, OnDestroy {

  @Output() modifyEntityId: EventEmitter<any> = new EventEmitter<any>();
  @Input() entityActiveTabName: string;

  isViewEntityDetails = false;
  entityList: any;
  $subscriptions: Subscription[] = [];


  constructor(private _router: Router, private _entityManagementService: EntityManagementService) { }

  ngOnInit() {
  }
  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  viewDetails(entityListData) {
    this._router.navigate(['/coi/entity-management/entity-list'], { queryParams: { entityManageId: entityListData.entityId } })
    // this.viewEntityDetails.emit(value);
  }

  viewListOfEntity() {
    this.$subscriptions.push(this._entityManagementService.getAllSystemEntityList().subscribe((res: any) => {
      this.entityList = res.coiEntityList;
    }));
  }
  ngOnChanges() {
    this.viewListOfEntity();
  }

  modifyEntityDetails(data) {
    this.modifyEntityId.emit(data.entityId);

  }
}
