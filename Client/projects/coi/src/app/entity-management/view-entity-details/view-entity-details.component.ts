import { Component, EventEmitter, OnDestroy, OnInit, Output } from '@angular/core';
import { ActivatedRoute, NavigationEnd, Router } from '@angular/router';
import { slowSlideInOut } from '../../../../../fibi/src/app/common/utilities/animations';
import { EntityManagementService } from '../entity-management.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { NavigationService } from '../../common/services/navigation.service';
import {environment} from "../../../environments/environment";

@Component({
  selector: 'app-view-entity-details',
  templateUrl: './view-entity-details.component.html',
  styleUrls: ['./view-entity-details.component.scss'],
  // animations: [slowSlideInOut]
})
export class ViewEntityDetailsComponent implements OnInit, OnDestroy {

  entityDetails: any = {};
  entityManageId: any;
  $subscriptions: Subscription[] = [];
  previousURL= '';
  deployMap = environment.deployUrl;
  imgURl = this.deployMap + 'assets/images/code-branch-solid.svg';

  constructor(private _router: Router, private _route: ActivatedRoute, public entityManagementService: EntityManagementService,
    private _commonServices: CommonService,private _navigationService:NavigationService) {
  }

  ngOnInit() {
    this.previousURL = this._navigationService.previousURL;
    this.entityManageId = this._route.snapshot.queryParamMap.get('entityManageId');
    this.getEntityDetails(this.entityManageId);
  }

  ngOnDestroy() {
    this.entityManagementService.isShowEntityNavBar = false;
    subscriptionHandler(this.$subscriptions);
  }

  backToList() {
    // this.hideEntityDetails.emit(false);
    this._router.navigateByUrl(this.previousURL)
  }

  modifyEntity() {
    this._router.navigate(['/coi/create-disclosure/entity-details']);
  }

  getEntityDetails(entityId) {
    this.$subscriptions.push(this.entityManagementService.getEntityDetails(entityId).subscribe((res: any) => {
      this.entityDetails = res.coiEntity;
    },_error=> {
      this._commonServices.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
    }));
  }


  updatedEntityDetails(event) {
    if(event) {
      this.getEntityDetails(this.entityManageId);
    }
  }


  redirectUrl(url) {
    if (url.includes('http')) {
      window.open(url, '_blank');
    } else {
      window.open('//' + url, '_blank');
    }
  }
}
