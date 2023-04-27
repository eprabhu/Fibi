import { Component, OnDestroy, OnInit } from '@angular/core';
import { EntityDetailsService } from '../entity-details.service';
import { ActivatedRoute, NavigationEnd, Router } from '@angular/router';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { NavigationService } from '../../../../../../fibi/src/app/common/services/navigation.service';
import { CommonService } from '../../../common/services/common.service';

@Component({
  selector: 'app-view-sfi-details',
  templateUrl: './view-sfi-details.component.html',
  styleUrls: ['./view-sfi-details.component.scss']
})
export class ViewSfiDetailsComponent implements OnInit, OnDestroy {

  isExpanded = true;
  entityDetails: any = {};
  $subscriptions: Subscription[] = [];

  constructor(public entityDetailsServices: EntityDetailsService, private _router: Router, private _route: ActivatedRoute,
    private _commonService: CommonService
  ) { }

  ngOnInit() {
    const ENTITY_ID = this._route.snapshot.queryParamMap.get('entityId')
    this.getEntityDetails(ENTITY_ID);
    // this.sfiServices.isShowSfiNavBar
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }
  getEntityDetails(entityId) {
    this.$subscriptions.push(this.entityDetailsServices.getCoiEntityDetails(entityId).subscribe((res: any) => {
      this.entityDetails = res.coiEntity;
      this.entityDetailsServices.entityDetails = res.coiEntity;
    }));
  }

  navigateBack() {
    this._commonService.previousURL ? this._router.navigateByUrl(this._commonService.previousURL) :
      this._router.navigate(['/coi/user-dashboard/entities']);
  }

}
