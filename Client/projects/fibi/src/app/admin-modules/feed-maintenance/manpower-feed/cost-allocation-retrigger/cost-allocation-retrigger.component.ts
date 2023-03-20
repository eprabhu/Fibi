import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { DEFAULT_DATE_FORMAT, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../app-constants';
import { CommonService } from '../../../../common/services/common.service';
import { ElasticConfigService } from '../../../../common/services/elastic-config.service';
import { setFocusToElement } from '../../../../common/utilities/custom-utilities';
import { parseDateWithoutTimestamp } from '../../../../common/utilities/date-utilities';
import { subscriptionHandler } from '../../../../common/utilities/subscription-handler';
import { AwardRetriggerListResponse, ManpowerFeedRequestObject } from '../manpower-feed.interface';
import { ManpowerFeedService } from '../manpower-feed.service';

declare var $: any;

@Component({
  selector: 'app-cost-allocation-retrigger',
  templateUrl: './cost-allocation-retrigger.component.html',
  styleUrls: ['./cost-allocation-retrigger.component.css']
})
export class CostAllocationRetriggerComponent implements OnInit, OnDestroy {

  $subscriptions: Subscription[] = [];
  setFocusToElement = setFocusToElement;
  datePlaceHolder = DEFAULT_DATE_FORMAT;
  searchObject: ManpowerFeedRequestObject = {
    startDate: null,
    endDate: null,
    currentPage: 1,
    itemsPerPage: 20,
    isDownload: false,
    budgetReferenceNumber: '',
    personId: '',
    tabName: 'COST_ALLOCATION_RETRIGGER',
    advancedSearch: 'L',
    sortBy: '',
    reverse: ''
  };
  allocationRetriggerList: AwardRetriggerListResponse;
  isFeedExpand: any = [];
  previousCostAllocations: any = [];
  userAction: any;
  actionComment: string;
  validationMap = new Map();
  retriggerObject: any;
  manualUpdateIndex: number;
  personElasticOptions: any = {};
  clearField: String;
  isDesc =  false;

  constructor(private _manpowerFeedService: ManpowerFeedService, public _commonService: CommonService,
    private _elasticConfig: ElasticConfigService) { }

  ngOnInit() {
    this.getPersonElasticOptions();
    this.getCostAllocationTriggerDetails();
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  getPersonElasticOptions(): void {
    this.personElasticOptions = this._elasticConfig.getElasticForPerson();
  }

  setPersonDetails(person: any): void {
    this.searchObject.personId = person ? person.prncpl_id : '';
  }

  getCostAllocationTriggerDetails(currentPage: number = 1, advanceSearch: string = 'L'): void {
    this.isFeedExpand = [];
    this.searchObject.currentPage = currentPage;
    this.searchObject.advancedSearch = advanceSearch;
    this.searchObject.startDate = parseDateWithoutTimestamp(this.searchObject.startDate);
    this.searchObject.endDate = parseDateWithoutTimestamp(this.searchObject.endDate);
    this.$subscriptions.push(
      this._manpowerFeedService.getCostAllocationTriggerDetails(this.searchObject).subscribe((data: any) => {
        this._commonService.isManualLoaderOn = false;
        this.allocationRetriggerList = data;
      }, err => {
        this._commonService.isManualLoaderOn = false;
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching cost allocation list failed. Please try again.');
      }
      )
    );
  }

  clearSearch(): void {
    this.searchObject.startDate = null;
    this.searchObject.endDate = null;
    this.searchObject.budgetReferenceNumber = '';
    this.searchObject.personId = '';
    this.searchObject.currentPage = 1;
    this.searchObject.itemsPerPage = 20;
    this.searchObject.isDownload = false;
    this.clearField = new String('true');
  }

  getCurrentCostAllocationDetails(personId: string, index: number, resourceUniqueId: string): void {
    if (this.isFeedExpand[index]) {
      this.$subscriptions.push(
        this._manpowerFeedService.getCurrentCostAllocationDetails({ personId: personId,
          resourceUniqueId: resourceUniqueId })
          .subscribe((data: any) => {
            this.previousCostAllocations[index] = data.workdayInterfaceLogDtos;
          }, err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching cost allocation failed. Please try again.'); }
          )
      );
    }
  }

  selectUserAction(code: string): void {
    this.userAction = this.allocationRetriggerList.manpowerUserActions.find(action => action.manpowerUserActionCode === code);
  }

  callSyncAPI(person: any, actionCode: string): void {
    this.selectUserAction(actionCode);
    this.retriggerObject = person;
    $('#syncCostAllocationAPI').modal('show');
  }

  clearAPIData(): void {
    this.retriggerObject = null;
    this.userAction = null;
    this.actionComment = null;
    this.isFeedExpand = [];
    $('#syncCostAllocationAPI').modal('hide');
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
      workdayManpowerInterfaceId: this.retriggerObject.workdayManpowerInterfaceId,
      manpowerLogId: this.retriggerObject.manpowerLogId,
      comments: this.actionComment,
      manpowerUserActionCode: this.userAction.manpowerUserActionCode
    };
  }

  retriggerWorkdayApi(): void {
    if (this.validateComment()) {
      this.$subscriptions.push(
        this._manpowerFeedService.retriggerWorkdayApi(this.requestObject())
          .subscribe((_data: any) => {
            this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Successfully Re-Triggered');
            this._commonService.isManualLoaderOn = true;
            this.getCostAllocationTriggerDetails(this.allocationRetriggerList.currentPage);
          }, err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching manpower retrigger list failed. Please try again.'); }
          )
      );
      this.clearAPIData();
    }
  }

  updateManpowerInterfaceManually(): void {
    if (this.validateComment()) {
      this.$subscriptions.push(
        this._manpowerFeedService.updateManpowerInterfaceManually({
          workdayManpowerInterfaceId: this.retriggerObject.workdayManpowerInterfaceId,
          comments: this.actionComment,
          manpowerUserActionCode: this.userAction.manpowerUserActionCode
        }).subscribe((data: any) => {
          this.updateManualProcessData(data);
          this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Successfully Updated Manually');
          this.clearAPIData();
        }, err => {
          this._commonService.showToast(HTTP_ERROR_STATUS, 'Manpower update process failed. Please try again.');
          this.clearAPIData();
        })
      );
    }
  }

  updateManualProcessData(data: any): void {
    this.allocationRetriggerList.workdayInterfaceLogDtos[this.manualUpdateIndex].comments = data.comments;
    this.allocationRetriggerList.workdayInterfaceLogDtos[this.manualUpdateIndex].manpowerInterfaceStatus = data.manpowerInterfaceStatus;
    this.allocationRetriggerList.workdayInterfaceLogDtos[this.manualUpdateIndex].interfaceStatusCode = data.interfaceStatusCode;
    this.allocationRetriggerList.workdayInterfaceLogDtos[this.manualUpdateIndex].userActionCode = data.manpowerUserActionCode;
    this.allocationRetriggerList.workdayInterfaceLogDtos[this.manualUpdateIndex].interfaceStatusName = data.parentInterfaceStatus;
  }

  public sortResult(sortByField) {
    this.isDesc = this.searchObject.sortBy === sortByField ? !this.isDesc : true;
    this.searchObject.reverse = this.isDesc === false ? 'DESC' : 'ASC';
    this.searchObject.sortBy = sortByField;
    this.getCostAllocationTriggerDetails();
  }

}
