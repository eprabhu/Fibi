import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output } from '@angular/core';
import { EntityDetailsService } from '../entity-details.service';
import { ActivatedRoute, Router } from '@angular/router';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { HTTP_ERROR_STATUS } from '../../../app-constants';
import { CommonService } from '../../../common/services/common.service';

@Component({
  selector: 'app-view-relationship-details',
  templateUrl: './view-relationship-details.component.html',
  styleUrls: ['./view-relationship-details.component.scss']
})
export class ViewRelationshipDetailsComponent implements  OnDestroy, OnChanges {


  isExpanded = true;
  relationshipsDetails: any = {};
  $subscriptions: Subscription[] = [];
  personEntityRelationships:any = [];
  @Input() updateRelationshipDetails :any;
  @Input() entityId :any;
  isReadMoreBusinessArea = false;
  isReadMoreUniversity = false;
  isReadMoreRelationWith = false;
  @Output() sfiRelationshipDetails: EventEmitter<any> = new EventEmitter<any>();

  constructor(public entityDetailsServices: EntityDetailsService, private _router: Router,
    private _route:ActivatedRoute,private _commonService: CommonService) { }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions)
  }

  ngOnChanges() {;
    this.getEntityDetails(this.getEntityId());
    if(this.updateRelationshipDetails) {
      this.personEntityRelationships.push(this.updateRelationshipDetails);
    }
  }

  getEntityId() {
    return this._route.snapshot.queryParamMap.get('personEntityId') || this.entityId;
  }

  getEntityDetails(personEntityId) {
    this.$subscriptions.push(this.entityDetailsServices.getRelationshipEntityDetails(personEntityId).subscribe((res: any) => {
      this.relationshipsDetails = res.personEntity;
      this.personEntityRelationships = res.personEntityRelationships;
      const relationSFiStatus = {
        isRelationshipActive: this.relationshipsDetails.isRelationshipActive,
        versionStatus: this.relationshipsDetails.versionStatus,
        personId : this.relationshipsDetails.personId
      };
      this.sfiRelationshipDetails.emit(relationSFiStatus);
    },_error=>{
      this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');

    }));
  }

  navigateBack() {
    this._router.navigate(['/coi/user-dashboard/entities'])
  }
}

