import { Component, EventEmitter, OnDestroy, OnInit, Output } from '@angular/core';
import { ActivatedRoute, NavigationEnd, Router } from '@angular/router';
import { slowSlideInOut } from '../../../../../fibi/src/app/common/utilities/animations';
import { EntityManagementService } from '../../entity-management/entity-management.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { NavigationService } from '../../common/services/navigation.service';
import { environment } from "../../../environments/environment";
import { EntityDetailsService } from '../../disclosure/entity-details/entity-details.service';
import { SfiService } from '../../disclosure/sfi/sfi.service';

@Component({
  selector: 'app-view-entity-sfi-details',
  templateUrl: './view-entity-details.component.html',
  styleUrls: ['./view-entity-details.component.scss'],
  // animations: [slowSlideInOut]
})
export class ViewEntityDetailsComponent implements OnInit, OnDestroy {

  entityDetails: any = {};
  entityId: any;
  $subscriptions: Subscription[] = [];
  previousURL = '';
  deployMap = environment.deployUrl;
  imgURl = this.deployMap + 'assets/images/code-branch-solid.svg';
  isEntityManagement = false;
  isModifyEntity = false;
  @Output() emitRelationshipModal: EventEmitter<boolean> = new EventEmitter<boolean>();
  @Output() saveQuestionnaire: EventEmitter<boolean> = new EventEmitter<boolean>();

  constructor(private _router: Router, private _route: ActivatedRoute, public entityManagementService: EntityManagementService,
    private _commonServices: CommonService, private _navigationService: NavigationService, public entityDetailsServices: EntityDetailsService, public sfiService: SfiService) {
  }

  ngOnInit() {
    this.isEntityManagement = this._router.url.includes('entity-management');
    this.isModifyEntity = this._commonServices.rightsArray.includes('MANAGE_ENTITY');
    this.entityId = this.isEntityManagement ? this._route.snapshot.queryParamMap.get('entityManageId') :
      this._route.snapshot.queryParamMap.get('entityId');
    this.getEntityDetails();
  }

  ngOnDestroy() {
    this.sfiService.isShowSfiNavBar = false;
    subscriptionHandler(this.$subscriptions);
  }

  navigateBack() {
    if (this.isEntityManagement) {
      this._navigationService.previousURL ? this._router.navigateByUrl(this._navigationService.previousURL) :
        this._router.navigate(['/coi/entity-management']);
    } else {
      this._navigationService.previousURL ? this._router.navigateByUrl(this._navigationService.previousURL) :
        this._router.navigate(['/coi/user-dashboard/entities']);
    }
  }

  getEntityDetails() {
    if (this.isEntityManagement) {
      this.viewEntityDetails();
    } else {
      this.getSfiEntityDetails();
    }
  }

  viewEntityDetails() {
    this.$subscriptions.push(this.entityManagementService.getEntityDetails(this.entityId).subscribe((res: any) => {
      this.entityDetails = res.coiEntity;
    }, _error => {
      this._commonServices.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
    }));
  }

  getSfiEntityDetails() {
    this.$subscriptions.push(this.entityDetailsServices.getCoiEntityDetails(this.entityId).subscribe((res: any) => {
      this.entityDetails = res.coiEntity;
      this.entityDetailsServices.entityDetailsId = res.coiEntity.entityId;
    }, _error => {
      this._commonServices.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
    }));
  }

  updatedEntityDetails(event) {
    if (event) {
      this.getEntityDetails();
    }
  }

  redirectUrl(url) {
    if (url.includes('http')) {
      window.open(url, '_blank');
    } else {
      window.open('//' + url, '_blank');
    }
  }

  addNewRelationship() {
    this.emitRelationshipModal.emit(true);
  }

  saveRelationship() {
    this.saveQuestionnaire.emit(true);
  }

}
