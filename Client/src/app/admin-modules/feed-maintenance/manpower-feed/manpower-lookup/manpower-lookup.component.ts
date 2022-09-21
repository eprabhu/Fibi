import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { DEFAULT_DATE_FORMAT, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../app-constants';
import { CommonService } from '../../../../common/services/common.service';
import { setFocusToElement } from '../../../../common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../../common/utilities/subscription-handler';
import { ManpowerInterface, ManpowerInterfaceType } from '../manpower-feed.interface';
import { ManpowerFeedService } from '../manpower-feed.service';

declare var $: any;

@Component({
  selector: 'app-manpower-lookup',
  templateUrl: './manpower-lookup.component.html',
  styleUrls: ['./manpower-lookup.component.css']
})
export class ManpowerLookupComponent implements OnInit, OnDestroy {

  setFocusToElement = setFocusToElement;
  manpowerInterface: ManpowerInterface[];
  interfaceType: ManpowerInterfaceType;
  $subscriptions: Subscription[] = [];
  datePlaceHolder = DEFAULT_DATE_FORMAT;

  constructor(private _manpowerFeedService: ManpowerFeedService, public _commonService: CommonService) { }

  ngOnInit() {
    this.fetchInterfaceApi();
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  private fetchInterfaceApi(): void {
    this.$subscriptions.push(
      this._manpowerFeedService.getManpowerLookupSyncDetails({}).subscribe((data: any) => {
        this.manpowerInterface = data;
      }, err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching manpower failed. Please try again.'); }
      )
    );
  }

  callSyncAPI(interfaceType: any): void {
    this.interfaceType = null;
    this.interfaceType = interfaceType;
    $('#syncManpowerAPI').modal('show');
  }

  syncAPI(): void {
    switch (this.interfaceType.interfaceTypeCode) {
      case '9': this.getManpowerDetails();
        break;
      case '10': this.getNationalityDetails();
        break;
      case '11': this.getCostingAllocationReconciliation();
        break;
      case '12': this.getWorkdayLongLeave();
        break;
      case '13': this.getWorkdayTerminations();
        break;
      case '15': this.getJobProfileChanges();
        break;
      case '16': this.getWorkdayJobProfile();
        break;
      default: return;
    }
  }

  private getWorkdayTerminations(): void {
    this.$subscriptions.push(
      this._manpowerFeedService.getWorkdayTerminations().subscribe((data: any) => {
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Workday Terminations Successfully synced.');
      }, err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Updating Workday Terminations failed. Please try again.'); }
      ));
  }

  private getWorkdayJobProfile(): void {
    this.$subscriptions.push(
      this._manpowerFeedService.getWorkdayJobProfile().subscribe((data: any) => {
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Workday Job Profile Successfully synced.');
      }, err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Updating Workday Job Profile failed. Please try again.'); }
      ));
  }

  private getManpowerDetails(): void {
    this.$subscriptions.push(
      this._manpowerFeedService.getManpowerDetails().subscribe((data: any) => {
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Workday Manpower Details Successfully synced.');
      }, err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Updating Workday Manpower Details failed. Please try again.'); }
      ));
  }

  private getNationalityDetails(): void {
    this.$subscriptions.push(
      this._manpowerFeedService.getNationalityDetails().subscribe((data: any) => {
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Workday Nationality Details Successfully synced.');
      }, err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Updating Workday Nationality Details failed. Please try again.'); }
      ));
  }

  private getJobProfileChanges(): void {
    this.$subscriptions.push(
      this._manpowerFeedService.getJobProfileChanges().subscribe((data: any) => {
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Workday Job Profile Successfully synced.');
      }, err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Updating Workday Job Profile failed. Please try again.'); }
      ));
  }

  private getWorkdayLongLeave(): void {
    this.$subscriptions.push(
      this._manpowerFeedService.getWorkdayLongLeave().subscribe((data: any) => {
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Workday Long LeaveSuccessfully synced.');
      }, err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Updating Workday Long Leave failed. Please try again.'); }
      ));
  }

  private getCostingAllocationReconciliation(): void {
    this.$subscriptions.push(
      this._manpowerFeedService.getCostingAllocationReconciliation().subscribe((data: any) => {
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Workday Cost Allocations Successfully synced.');
      }, err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Updating Workday Cost Allocations failed. Please try again.'); }
      ));
  }

}
