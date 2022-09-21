import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { DEFAULT_DATE_FORMAT, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../app-constants';
import { CommonService } from '../../../../common/services/common.service';
import { setFocusToElement } from '../../../../common/utilities/custom-utilities';
import { parseDateWithoutTimestamp } from '../../../../common/utilities/date-utilities';
import { subscriptionHandler } from '../../../../common/utilities/subscription-handler';
import { AwardRetriggerListResponse, ManpowerFeedRequestObject, ManpowerUserAction } from '../manpower-feed.interface';
import { ManpowerFeedService } from '../manpower-feed.service';

declare var $: any;

@Component({
  selector: 'app-award-retrigger',
  templateUrl: './award-retrigger.component.html',
  styleUrls: ['./award-retrigger.component.css']
})
export class AwardRetriggerComponent implements OnInit, OnDestroy {

  setFocusToElement = setFocusToElement;
  datePlaceHolder = DEFAULT_DATE_FORMAT;
  searchObject: ManpowerFeedRequestObject = {
    startDate: null,
    endDate: null,
    awardNumber: null,
    currentPage: 1,
    itemsPerPage: 20,
    isDownload: false,
    advancedSearch: 'L',
    tabName: 'AWARD_MANPOWER_RETRIGGER',
    sortBy: '',
    reverse : ''
  };
  awardRetriggerList: AwardRetriggerListResponse;
  $subscriptions: Subscription[] = [];
  workdayManpowerInterfaceId: string;
  userAction: ManpowerUserAction;
  actionComment: string;
  validationMap = new Map();
  manualUpdateIndex: number;
  isDesc = false;

  constructor(private _manpowerFeedService: ManpowerFeedService, public _commonService: CommonService) { }

  ngOnInit() {
    this.fetchAwardRetriggerList();
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  fetchAwardRetriggerList(currentPage: number = 1): void {
    this.searchObject.currentPage = currentPage;
    this.searchObject.startDate = parseDateWithoutTimestamp(this.searchObject.startDate);
    this.searchObject.endDate = parseDateWithoutTimestamp(this.searchObject.endDate);
    this.$subscriptions.push(
      this._manpowerFeedService.getAwardTriggerDetails(this.searchObject).subscribe((data: any) => {
        this.awardRetriggerList = data;
      }, err => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching award list for retrigger failed. Please try again.');
      }
      )
    );
  }

  clearSearch(): void {
    this.searchObject.startDate = null;
    this.searchObject.endDate = null;
    this.searchObject.awardNumber = null;
    this.searchObject.currentPage = 1;
    this.searchObject.itemsPerPage = 20;
    this.searchObject.isDownload = false;
    this.searchObject.advancedSearch = 'L';
  }

  retriggerRequestObject(): any {
    return {
      workdayManpowerInterfaceId: this.workdayManpowerInterfaceId,
      comments: this.actionComment,
      manpowerUserActionCode: this.userAction.manpowerUserActionCode,
      startDate: this.searchObject.startDate,
      endDate: this.searchObject.endDate,
      awardNumber: this.searchObject.awardNumber,
      currentPage: this.searchObject.currentPage,
      itemsPerPage: this.searchObject.itemsPerPage,
      isDownload: this.searchObject.isDownload
    };
  }

  retriggerAwardWorkdayPrerequisite(): void {
    if (this.validateComment()) {
      this.$subscriptions.push(
        this._manpowerFeedService.retriggerAwardWorkdayPrerequisite(this.retriggerRequestObject())
        .subscribe((data: any) => {
          this.awardRetriggerList.workdayInterfaceLogDtos = data.workdayInterfaceLogDtos;
          this.awardRetriggerList.pageCount = data.pageCount;
          this.awardRetriggerList.itemsPerPage = data.itemsPerPage;
          this.awardRetriggerList.currentPage = data.currentPage;
          this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Successfully Re-Triggered');
          this.clearAPIData();
        }, err => {
          this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching award list for retrigger failed. Please try again.');
          this.clearAPIData();
        })
      );
    }
  }

  validateComment(): boolean {
    this.validationMap.clear();
    if (!this.actionComment) {
      this.validationMap.set('comment', 'Enter comment');
    }
    return this.validationMap.size ? false : true;
  }

  selectUserAction(code: string): void {
    this.userAction = this.awardRetriggerList.manpowerUserActions.find(action => action.manpowerUserActionCode === code);
  }

  clearAPIData(): void {
    this.manualUpdateIndex = null;
    this.workdayManpowerInterfaceId = null;
    this.userAction = null;
    this.actionComment = null;
    $('#syncAwardManpowerAPI').modal('hide');
  }

  updateManpowerInterfaceManually(): void {
    if (this.validateComment()) {
      this.$subscriptions.push(
        this._manpowerFeedService.updateManpowerInterfaceManually({
          workdayManpowerInterfaceId: this.workdayManpowerInterfaceId,
          comments: this.actionComment,
          manpowerUserActionCode: this.userAction.manpowerUserActionCode
        }).subscribe((data: any) => {
          this.updateManualProcessData(data);
          this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Successfully Updated Manually');
          this.clearAPIData();
        }, err => {
          this._commonService.showToast(HTTP_ERROR_STATUS, 'Updating manpower interface failed. Please try again.');
          this.clearAPIData();
        })
      );
    }
  }

  updateManualProcessData(data: any): void {
    this.awardRetriggerList.workdayInterfaceLogDtos[this.manualUpdateIndex].comments = data.comments;
    this.awardRetriggerList.workdayInterfaceLogDtos[this.manualUpdateIndex].manpowerInterfaceStatus = data.manpowerInterfaceStatus;
    this.awardRetriggerList.workdayInterfaceLogDtos[this.manualUpdateIndex].interfaceStatusCode = data.interfaceStatusCode;
    this.awardRetriggerList.workdayInterfaceLogDtos[this.manualUpdateIndex].userActionCode = data.manpowerUserActionCode;
  }

  public sortResult(sortByField) {
    this.isDesc = this.searchObject.sortBy === sortByField ? !this.isDesc : true;
    this.searchObject.reverse = this.isDesc === false ? 'DESC' : 'ASC';
    this.searchObject.sortBy = sortByField;
    this.fetchAwardRetriggerList();
  }

}
