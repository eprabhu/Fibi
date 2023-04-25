import { Component, EventEmitter, OnDestroy, OnInit, Output } from '@angular/core';
import { ActivatedRoute, NavigationEnd, Router } from '@angular/router';
import { slowSlideInOut } from '../../../../../fibi/src/app/common/utilities/animations';
import { EntityManagementService } from '../entity-management.service';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';

@Component({
  selector: 'app-view-entity-details',
  templateUrl: './view-entity-details.component.html',
  styleUrls: ['./view-entity-details.component.scss'],
  animations: [slowSlideInOut]
})
export class ViewEntityDetailsComponent implements OnInit, OnDestroy {

  entityDetails: any = {};
  entityManageId: any;
  $subscriptions: Subscription[] = [];

  constructor(private _router: Router, private _route: ActivatedRoute, public entityManagementService: EntityManagementService,
    private _commonServices: CommonService) {
    this.getPreviousURL();
  }

  ngOnInit() {
    this.entityManageId = this._route.snapshot.queryParamMap.get('entityManageId');
    this.getEntityDetails(this.entityManageId);
  }

  ngOnDestroy() {
    this.entityManagementService.isShowEntityNavBar = false;
    subscriptionHandler(this.$subscriptions);
  }

  backToList() {
    // this.hideEntityDetails.emit(false);
    this._router.navigate(['/coi/entity-management']);
  }

  modifyEntity() {
    this._router.navigate(['/coi/create-disclosure/entity-details']);
  }

  getEntityDetails(entityId) {
    this.$subscriptions.push(this.entityManagementService.getEntityDetails(entityId).subscribe((res: any) => {
      this.entityDetails = res.coiEntity;
    }));
  }

  getPreviousURL() {
    this.$subscriptions.push(this._router.events.subscribe(event => {
      if (event instanceof NavigationEnd) {
        this._commonServices.previousURL = event.url;
      }
    }));
  }
  updatedEntityDetails(event) {
    if(event) {
      this.getEntityDetails(this.entityManageId);
    }
  }
}
