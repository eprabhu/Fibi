import { Component, OnInit, OnDestroy } from '@angular/core';

import { CommonService } from '../../common/services/common.service';
import { ElasticConfigService } from '../../common/services/elastic-config.service';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { pageScroll } from '../../common/utilities/custom-utilities';
import { DisclosureListService } from './disclosure-list.service';

@Component({
  selector: 'app-disclosure-list',
  templateUrl: './disclosure-list.component.html',
  styleUrls: ['./disclosure-list.component.css']
})
export class DisclosureListComponent implements OnInit, OnDestroy {
  isShowResultCard = false;
  isShowAdvanceSearchOptions = false;
  isReverse = true;

  elasticResultObject: any = {};
  elasticSearchOptions: any = {};
  serviceRequestList: any = [];
  result: any = {};
  $subscriptions: Subscription[] = [];
  disclosureRequestObject = this._disclosurelistService.getDashboardObject();
  pageScroll = pageScroll;

  constructor(private _disclosurelistService: DisclosureListService, public _commonService: CommonService,
    private _elasticConfig: ElasticConfigService) { }

  ngOnInit() {
    this.disclosureRequestObject.tabIndex = 'DISCLOSURE';
    this.disclosureRequestObject.sort = {};
    this.disclosureRequestObject.currentPage = 1;
    if (!this.disclosureRequestObject.isUnitAdmin) {
      this.loadDashboard();
    }
    this.elasticSearchOptions = this._elasticConfig.getElasticForCoi();
  }
  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }
  /* fetch DISCLOSURE list */
  loadDashboard() {
    this.$subscriptions.push(this._disclosurelistService.getDashboardList(this.disclosureRequestObject)
      .subscribe(data => {
        this.result = data || [];
        if (this.result != null) {
          this.serviceRequestList = this.result.disclosureViews;
        }
      }));
  }

  actionsOnPageChange(event) {
    this.disclosureRequestObject.currentPage = event;
    this.loadDashboard();
    pageScroll('pageScrollToTop');
  }
  /** show and hide advance search feature
   * @param event
   */
  showAdvanceSearch(event: any) {
    event.preventDefault();
    this.isShowAdvanceSearchOptions = !this.isShowAdvanceSearchOptions;
    this.clear();
  }

  /** searches using advance search options */
  searchUsingAdvanceOptions() {
    /* close elastic search result if it is open */
    if (this.isShowResultCard === true) {
      this.isShowResultCard = false;
    }
    this.loadDashboard();
  }

  /** clear all advanced search fields */
  clear() {
    this.disclosureRequestObject.property1 = '';
    this.disclosureRequestObject.property2 = '';
    this.disclosureRequestObject.property3 = '';
    this.disclosureRequestObject.property4 = '';
  }

  /** select a result from elastic search
  * @param value
  */
  selectDisclosureElasticResult(value) {
    if (value) {
      this.isShowResultCard = true;
      this.elasticResultObject = value;
    } else {
      this.isShowResultCard = false;
      this.elasticResultObject = {};
    }
  }

  /** sorts results based on fields
   * @param sortFieldBy
   */
  sortResult(sortFieldBy) {
    this.isReverse = (this.disclosureRequestObject.sortBy === sortFieldBy) ? !this.isReverse : false;
    if (this.isReverse) {
      this.disclosureRequestObject.reverse = 'DESC';
    } else {
      this.disclosureRequestObject.reverse = 'ASC';
    }
    this.disclosureRequestObject.sortBy = sortFieldBy;
    this.loadDashboard();
  }
}
