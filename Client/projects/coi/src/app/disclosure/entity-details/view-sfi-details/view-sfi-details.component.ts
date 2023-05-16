import { Component, EventEmitter, OnDestroy, OnInit, Output } from '@angular/core';
import { EntityDetailsService } from '../entity-details.service';
import { ActivatedRoute, NavigationEnd, Router } from '@angular/router';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { HTTP_ERROR_STATUS } from '../../../app-constants';
import { NavigationService } from '../../../common/services/navigation.service';

@Component({
  selector: 'app-view-sfi-details',
  templateUrl: './view-sfi-details.component.html',
  styleUrls: ['./view-sfi-details.component.scss']
})
export class ViewSfiDetailsComponent implements OnInit, OnDestroy {

  isExpanded = true;
  entityDetails: any = {};
  $subscriptions: Subscription[] = [];
  prevURL = '';
  @Output() emitRelationshipModal: EventEmitter<boolean> = new EventEmitter<boolean>();
  @Output() saveQuestionnaire: EventEmitter<boolean> = new EventEmitter<boolean>();

  constructor(public entityDetailsServices: EntityDetailsService, private _router: Router, private _route: ActivatedRoute,
    private _commonService: CommonService, private _navigationService:NavigationService
  ) { }

  ngOnInit() {
    const ENTITY_ID = this._route.snapshot.queryParamMap.get('entityId')
    this.getEntityDetails(ENTITY_ID);
    this.prevURL = this._navigationService.previousURL;
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }
  getEntityDetails(entityId) {
    this.$subscriptions.push(this.entityDetailsServices.getCoiEntityDetails(entityId).subscribe((res: any) => {
      this.entityDetails = res.coiEntity;
      this.entityDetailsServices.entityDetails = res.coiEntity;
    },_error=>{
      this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
    }));
  }

  navigateBack() {
    this.prevURL ? this._router.navigateByUrl(this.prevURL) : this._router.navigate(['/coi/user-dashboard/entities']);
  }

  redirectUrl(url) {
    if (url.includes('http')) {
      window.open(url, '_blank');
    } else {
      window.open('//' + url, '_blank');
    }
  }

  addNewRelationship(){
    this.emitRelationshipModal.emit(true);
  }

  saveRelationship(){
    this.saveQuestionnaire.emit(true);
  }
}
