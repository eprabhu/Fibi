import { Component, OnDestroy, OnInit } from '@angular/core';
import { EntityDetailsService } from '../entity-details.service';
import { ActivatedRoute, Router } from '@angular/router';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';

@Component({
  selector: 'app-view-relationship-details',
  templateUrl: './view-relationship-details.component.html',
  styleUrls: ['./view-relationship-details.component.scss']
})
export class ViewRelationshipDetailsComponent implements OnInit, OnDestroy {


  isExpanded = true;
  relationshipsDetails: any = {};
  $subscriptions: Subscription[] = [];

  constructor(public entityDetailsServices: EntityDetailsService, private _router: Router,
    private _route:ActivatedRoute) { }

  ngOnInit() {
    const ENTITY_ID = this._route.snapshot.queryParamMap.get('entityId');
    this.getEntityDetails(ENTITY_ID);
    // this.sfiServices.isShowSfiNavBar
  }
  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions)
  }

  getEntityDetails(personEntityId) {
    this.$subscriptions.push(this.entityDetailsServices.getRelationshipEntityDetails(personEntityId).subscribe((res: any) => {
      this.relationshipsDetails = res.personEntity;
    }));
  }

  navigateBack() {
    this._router.navigate(['/coi/user-dashboard/entities'])
    // this._router.navigateByUrl(this.entityDetailsServices.previousURL);
  }
}

