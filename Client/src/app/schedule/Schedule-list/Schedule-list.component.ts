import { Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';

import { setFocusToElement } from '../../common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { pageScroll } from '../../common/utilities/custom-utilities';
import { ScheduleListService } from './Schedule-list.service';

@Component({
  selector: 'app-Schedule-list',
  templateUrl: './Schedule-list.component.html',
  styleUrls: ['./Schedule-list.component.css']
})
export class ScheduleListComponent implements OnInit, OnDestroy {
  filterStartDate: Date;
  filterEndDate: Date;
  isReverse = true;
  isValid = true;
  errorMsg = '';
  serviceRequestList: any = [];
  result: any = {};
  setFocusToElement = setFocusToElement;
  $subscriptions: Subscription[] = [];
  requestObject = this._schedulelistService.getDashboardObject();
  pageScroll = pageScroll;

  constructor(private _schedulelistService: ScheduleListService, private _router: Router) { }

  ngOnInit() {
    this.requestObject.tabIndex = 'SCHEDULE';
    this.requestObject.sort = {};
    this.requestObject.currentPage = 1;
    this.loadDashboard();
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }
  /* fetch SCHEDULE list */
  loadDashboard() {
    this.$subscriptions.push(this._schedulelistService.getDashboardList(this.requestObject)
      .subscribe(data => {
        this.result = data || [];
        if (this.result != null) {
          this.serviceRequestList = this.result.committeeSchedules;
        }
      }));
  }

  actionsOnPageChange(event) {
    this.requestObject.currentPage = event;
    this.loadDashboard();
    pageScroll('pageScrollToTop');
  }
  /* filter schedule based on dates */
  filterSchedule() {
    if (this.filterStartDate > this.filterEndDate) {
      this.isValid = false;
      this.errorMsg = '* Please ensure that the To : Date is greater than or equal to the From : Date.';
    } else if (this.filterStartDate === null || this.filterStartDate === undefined ||
      this.filterEndDate === null || this.filterEndDate === undefined) {
      this.isValid = false;
      this.errorMsg = '* Please enter the necessary dates to apply filter.';
    }
    if (this.isValid) {
      this.loadDashboard();
    }
  }

  /* clear filter schedule dates */
  clearFilterSchedule() {
    this.filterStartDate = this.filterEndDate = null;
    this.isValid = true;
    this.loadDashboard();
  }
  /* view schedule */
  loadSchedules(event: any, scheduleId) {
    event.preventDefault();
    this._router.navigate(['committee/schedule'], { queryParams: { 'scheduleId': scheduleId } });
  }

  /** sorts results based on fields
    * @param sortFieldBy
    */
  sortResult(sortFieldBy) {
    this.isReverse = (this.requestObject.sortBy === sortFieldBy) ? !this.isReverse : false;
    if (this.isReverse) {
      this.requestObject.reverse = 'DESC';
    } else {
      this.requestObject.reverse = 'ASC';
    }
    this.requestObject.sortBy = sortFieldBy;
    this.loadDashboard();
  }
}
