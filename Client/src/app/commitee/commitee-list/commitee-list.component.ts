import { Component, OnInit, OnDestroy } from '@angular/core';

import { DashboardService } from '../../dashboard/dashboard.service';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { pageScroll } from '../../common/utilities/custom-utilities';
import { CommiteeListService } from './commitee-list.service';

@Component({
  selector: 'app-commitee-list',
  templateUrl: './commitee-list.component.html',
  styleUrls: ['./commitee-list.component.css']
})
export class CommiteeListComponent implements OnInit, OnDestroy {

  isReverse = true;
  serviceRequestList = [];
  result: any = {};
  $subscriptions: Subscription[] = [];
  committeeRequestObject = this._commiteelistService.getDashboardObject();
  pageScroll = pageScroll;
  sortBy = '';

  constructor(private _commiteelistService: CommiteeListService) { }

  ngOnInit() {
    this.committeeRequestObject.tabIndex = 'COMMITTEE';
    this.committeeRequestObject.sort = {};
    this.committeeRequestObject.currentPage = 1;
    this.loadDashboard();
  }
  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  /** fetch committee list */
  loadDashboard() {
    this.$subscriptions.push(this._commiteelistService.getDashboardList(this.committeeRequestObject)
      .subscribe(data => {
        this.result = data || [];
        if (this.result !== null) {
          this.serviceRequestList = this.result.committees;
        }
      }));
  }
  actionsOnPageChange(event) {
    this.committeeRequestObject.currentPage = event;
    this.loadDashboard();
    pageScroll('pageScrollToTop');
  }
  /** sorts results based on fields
   * @param sortFieldBy
   */
  sortResult(sortFieldBy) {
    this.isReverse = (this.committeeRequestObject.sortBy === sortFieldBy) ? !this.isReverse : false;
    if (this.isReverse) {
      this.committeeRequestObject.reverse = 'DESC';
    } else {
      this.committeeRequestObject.reverse = 'ASC';
    }
    this.committeeRequestObject.sortBy = sortFieldBy;
    this.loadDashboard();
  }
}
