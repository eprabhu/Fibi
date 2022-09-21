import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { DEFAULT_DATE_FORMAT, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../app-constants';
import { CommonService } from '../../../../common/services/common.service';
import { slideInOut } from '../../../../common/utilities/animations';
import { setFocusToElement } from '../../../../common/utilities/custom-utilities';
import { parseDateWithoutTimestamp } from '../../../../common/utilities/date-utilities';
import { subscriptionHandler } from '../../../../common/utilities/subscription-handler';
import { AwardRetriggerListResponse, ManpowerFeedRequestObject, ManpowerLog, ManpowerUserAction } from '../manpower-feed.interface';
import { ManpowerFeedService } from '../manpower-feed.service';

declare var $: any;

@Component({
  selector: 'app-position-retrigger',
  templateUrl: './position-retrigger.component.html',
  styleUrls: ['./position-retrigger.component.css'],
  animations: [slideInOut]
})
export class PositionRetriggerComponent implements OnInit, OnDestroy {

  setFocusToElement = setFocusToElement;
  datePlaceHolder = DEFAULT_DATE_FORMAT;
  searchObject: ManpowerFeedRequestObject = {
    startDate: null,
    endDate: null,
    awardNumber: '',
    positionId: '',
    currentPage: 1,
    itemsPerPage: 20,
    isDownload: false,
    tabName: 'POSITION_RETRIGGER',
    advancedSearch: 'L',
    sortBy: '',
    reverse: ''
  };
  positionRetriggerList: AwardRetriggerListResponse;
  isFeedExpand: any = [];
  positionErrorList: ManpowerLog[] = [];
  manpowerLogData: any = {};
  actionComment: string;
  userAction: ManpowerUserAction;
  validationMap = new Map();
  $subscriptions: Subscription[] = [];
  resourceUniqueId: string;
  expandIndex: number;
  manualUpdateIndex: number;
  isDesc = false;

  constructor(private _manpowerFeedService: ManpowerFeedService, public _commonService: CommonService) { }

  ngOnInit() {
    this.fetchPositionRetriggerList();
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  fetchPositionRetriggerList(currentPage: number = 1, advanceSearch: string = 'L'): void {
    this.isFeedExpand = [];
    this.searchObject.currentPage = currentPage;
    this.searchObject.advancedSearch = advanceSearch;
    this.searchObject.startDate = parseDateWithoutTimestamp(this.searchObject.startDate);
    this.searchObject.endDate = parseDateWithoutTimestamp(this.searchObject.endDate);
    this.$subscriptions.push(
      this._manpowerFeedService.getPositionTriggerDetails(this.searchObject).subscribe((data: any) => {
        this.positionRetriggerList = data;
      }, err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching Position Re-trigger list failed. Please try again.'); }
      )
    );
  }

  clearSearch(): void {
    this.searchObject.startDate = null;
    this.searchObject.endDate = null;
    this.searchObject.awardNumber = '';
    this.searchObject.positionId = '';
    this.searchObject.currentPage = 1;
    this.searchObject.itemsPerPage = 20;
    this.searchObject.isDownload = false;
  }

  fetchPositionErrors(resourceUniqueId: string, index: number): void {
    if (this.isFeedExpand[index]) {
      this.$subscriptions.push(
        this._manpowerFeedService.getPositionErrorDetails({ resourceUniqueId: resourceUniqueId })
          .subscribe((data: any) => {
            this.positionErrorList[index] = data.workdayInterfaceLogDtos;
          }, err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching Position Error list failed. Please try again.'); }
          )
      );
    }
  }

  selectUserAction(code: string): void {
    this.userAction = this.positionRetriggerList.manpowerUserActions.find(action => action.manpowerUserActionCode === code);
  }

  callSyncAPI(log: any, actionCode: string): void {
    this.selectUserAction(actionCode);
    this.manpowerLogData = log;
    $('#syncManpowerPositionAPI').modal('show');
  }

  clearAPIData(): void {
    this.manpowerLogData = null;
    this.userAction = null;
    this.actionComment = null;
    $('#syncManpowerPositionAPI').modal('hide');
  }

  validateComment(): boolean {
    this.validationMap.clear();
    if (!this.actionComment) {
      this.validationMap.set('comment', 'Enter comment');
    }
    return this.validationMap.size ? false : true;
  }

  requestObject(): any {
    return {
      workdayManpowerInterfaceId: this.manpowerLogData.workdayManpowerInterfaceId,
      manpowerLogId: this.manpowerLogData.manpowerLogId,
      comments: this.actionComment,
      manpowerUserActionCode: this.userAction.manpowerUserActionCode
    };
  }

  retriggerWorkdayApi(): void {
    if (this.validateComment()) {
      this.$subscriptions.push(
        this._manpowerFeedService.retriggerWorkdayApi(this.requestObject())
          .subscribe((data: any) => {
            this.retriggerResponse(data);
          }, err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching Position Re-trigger list failed. Please try again.'); }
          )
      );
      this.clearAPIData();
    }
  }

  updateManpowerInterfaceManually(): void {
    if (this.validateComment()) {
      this.$subscriptions.push(
        this._manpowerFeedService.updateManpowerInterfaceManually({
          workdayManpowerInterfaceId: this.manpowerLogData.workdayManpowerInterfaceId,
          comments: this.actionComment,
          manpowerUserActionCode: this.userAction.manpowerUserActionCode
        }).subscribe((data: any) => {
          this.updateManualProcessData(data, this.positionErrorList[this.expandIndex]);
          this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Successfully Updated Manually');
          this.clearAPIData();
        }, err => {
          this._commonService.showToast(HTTP_ERROR_STATUS, 'Updating manpower ststus failed. Please try again.');
          this.clearAPIData();
        })
      );
    }
  }

  updateManualProcessData(data: any, updatedValue: any = []): void {
    updatedValue[this.manualUpdateIndex].comments = data.comments;
    updatedValue[this.manualUpdateIndex].manpowerInterfaceStatus = data.manpowerInterfaceStatus;
    updatedValue[this.manualUpdateIndex].interfaceStatusCode = data.interfaceStatusCode;
    updatedValue[this.manualUpdateIndex].userActionCode = data.manpowerUserActionCode;
    updatedValue[this.manualUpdateIndex].userActionName = data.manpowerUserAction;
    this.updateParentInterfaceStatus(data);
  }

  updateParentInterfaceStatus(data: any): void {
    this.positionRetriggerList.workdayInterfaceLogDtos[this.expandIndex].interfaceStatusName = data.parentInterfaceStatus;
  }

  initiateRetriggerApi(): void {
    this.manpowerLogData.interfaceTypeCode === '3' ? this.retriggerWorkdayClosePosition() : this.retriggerWorkdayApi();
  }

  retriggerWorkdayClosePosition(): void {
    if (this.validateComment()) {
      this.$subscriptions.push(
        this._manpowerFeedService.retriggerWorkdayClosePosition(this.requestObject())
          .subscribe((data: any) => {
            this.retriggerResponse(data);
          }, err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching Close Position Re-trigger list failed. Please try again.'); }
          )
      );
      this.clearAPIData();
    }
  }

  retriggerResponse(data: any): void {
    this.positionErrorList[this.expandIndex] = data.workdayInterfaceLogDtos;
    this.updateParentInterfaceStatus(data);
    this.expandIndex = null;
    this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Successfully re-triggered.');
  }

  public sortResult(sortByField) {
    this.isDesc = this.searchObject.sortBy === sortByField ? !this.isDesc : true;
    this.searchObject.reverse = this.isDesc === false ? 'DESC' : 'ASC';
    this.searchObject.sortBy = sortByField;
    this.fetchPositionRetriggerList();
  }

}
