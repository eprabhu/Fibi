import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { DEFAULT_DATE_FORMAT } from '../../app-constants';
import { fadeDown } from '../../common/utilities/animations';
import { Constants } from '../../common/constants/action-list.constants';
import { CommonService } from '../../common/services/common.service';
import { setFocusToElement } from '../../common/utilities/custom-utilities';
import { getTimeInterval, parseDateWithoutTimestamp } from '../../common/utilities/date-utilities';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { ResearchSummaryWidgetsService } from '../research-summary-widgets.service';
import { Router } from '@angular/router';

@Component({
  selector: 'app-action-list',
  templateUrl: './action-list.component.html',
  styleUrls: ['./action-list.component.css'],
  animations: [fadeDown]
})
export class ActionListComponent implements OnInit, OnDestroy {

  inboxDetails: any = [];
  $subscriptions: Subscription[] = [];
  modulePath = Object.assign({}, Constants.paths);
  getTimeInterval = getTimeInterval;
  viewInboxSearch = false;
  inboxTab = 'PENDING';
  inboxObject: any = {
    moduleCode: null
  };
  datePlaceHolder = DEFAULT_DATE_FORMAT;
  setFocusToElement = setFocusToElement;
  moduleList: any = [];
  isInboxInfo = true;
  filterFields: any = [];
  isSaving = false;
  widgetDescription: any;

  constructor(private _researchsummaryService: ResearchSummaryWidgetsService, public _commonService: CommonService,
    private _router:  Router) { }

  ngOnInit() {
    this.widgetDescription = this._researchsummaryService.getWidgetDescription(2);
    this.getInboxTab();
  }

  getActionList(type) {
    if (!this.isSaving) {
      this.isSaving = true;
      this.inboxObject.toPersonId = this._commonService.getCurrentUserDetail('personID');
      this.inboxObject.isViewAll = 'Y';
      this.inboxObject.processed = type;
      this.inboxObject.fromDate = parseDateWithoutTimestamp(this.inboxObject.fromDate);
      this.inboxObject.toDate = parseDateWithoutTimestamp(this.inboxObject.toDate);
      this.$subscriptions.push(this._researchsummaryService.getActionList(this.inboxObject).subscribe((data: any) => {
        this.inboxDetails = data.inboxDetails;
        this.moduleList = data.modules;
        this.setListWithModule();
        this.isSaving = false;
      }, err => { this.isSaving = false; }));
    }
  }

  setListWithModule() {
    this.inboxDetails.forEach(element => {
      Object.keys(this.modulePath).forEach(key => {
        if (key === this.getModulePathKey(element)) {
          element.class = this.modulePath[key].class;
          element.name = this.modulePath[key].name;
        }
      });
    });
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  getModulePathKey(el) {
    return el.moduleCode !== 1 ? el.moduleCode.toString() : el.moduleCode.toString() + this.getAwardSubmoduleCode(el);
  }

  getAwardSubmoduleCode(el) {
    return el.subModuleCode ? el.subModuleCode.toString() : '0';
  }

  getSubModulePath(el) {
    return this.modulePath[this.getModulePathKey(el)].subPath ? this.modulePath[this.getModulePathKey(el)].subPath : '';
  }

  getSubModuleKey(el) {
    return this.modulePath[this.getModulePathKey(el)].subPath ? el.subModuleItemKey : '';
  }

  goToActionPath(inbox, i) {
    this._commonService.isPreventDefaultLoader = false;
    if(inbox.moduleCode.toString() === '3'){
      if(inbox.messageTypeCode === '105') {
        localStorage.setItem('currentTab', 'PROPOSAL_HOME');
      } else {
        localStorage.setItem('currentTab', 'PROPOSAL_REVIEW');
        this._router.navigate(['fibi/proposal/summary'],{ queryParams: { proposalId: inbox.moduleItemKey } });
        return;
      }
    }
    window.open(window.location.origin + window.location.pathname + this.modulePath[this.getModulePathKey(inbox)].path
      + inbox.moduleItemKey + this.getSubModulePath(inbox) + this.getSubModuleKey(inbox), '_self');
  }

  markAsRead(id, index) {
    this.$subscriptions.push(this._researchsummaryService.openUnreadInbox(id).subscribe(data => { }));
  }

  getInboxTab() {
    this.inboxTab === 'PENDING' ? this.getActionList(false) : this.getActionList(true);
    this.viewInboxSearch = false;
    // this.isInboxInfo = false;
    // this.clearInboxSearchField();
  }

  clearInboxSearchField() {
    this.inboxObject.moduleCode = null;
  }

}
