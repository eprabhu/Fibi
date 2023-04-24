import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Subject, Subscription } from 'rxjs';
import { switchMap } from 'rxjs/operators';
import { ElasticConfigService } from '../../../../fibi/src/app/common/services/elastic-config.service';
import { getEndPointOptionsForEntity, getEndPointOptionsForLeadUnit } from '../../../../fibi/src/app/common/services/end-point.config';
import { isEmptyObject } from '../../../../fibi/src/app/common/utilities/custom-utilities';
import { parseDateWithoutTimestamp } from '../../../../fibi/src/app/common/utilities/date-utilities';
import { ReviewerDashboardRequest, ReviewerDashboardService, SortCountObj } from './reviewer-dashboard.service';

@Component({
  selector: 'app-reviewer-dashboard',
  templateUrl: './reviewer-dashboard.component.html',
  styleUrls: ['./reviewer-dashboard.component.scss']
})
export class ReviewerDashboardComponent implements OnInit {

  coiElastic: any;
  dashboardCounts = {
    conflictIdentifiedCount: 0,
    pendingEntityApproval: 0
  };
  EntitySearchOptions: any = {};
  elasticPersonSearchOptions: any = {};
  clearField: any;
  lookupValues = [];
  leadUnitSearchOptions: any = {};
  advSearchClearField: String;
  disclosureStatusOptions = 'coi_disclosure_status#DISCLOSURE_STATUS_CODE#true#true';
  disclosureTypeOptions = 'coi_disclosure_category_type#DISCLOSURE_CATEGORY_TYPE_CODE#true#true';
  advanceSearchDates = { certificationDate: null, expirationDate: null };
  $subscriptions: Subscription[] = [];
  $coiList = new Subject();
  result: any = { disclosureCount: 0 };
  coiList = [];
  isShowAllProposalList = false;
  sortMap: any = {};
  sortCountObj: any = {};
  isActiveDisclosureAvailable: boolean;

  constructor(public reviewerDashboardService: ReviewerDashboardService,
    private _router: Router, private _elasticConfig: ElasticConfigService) { }


  ngOnInit() {
    this.sortCountObj = new SortCountObj();
    this.getCount();
    this.getDashboardDetails();
    this.coiElastic = this._elasticConfig.getElasticForCoi();
    this.EntitySearchOptions = getEndPointOptionsForEntity();
    this.elasticPersonSearchOptions = this._elasticConfig.getElasticForPerson();
    this.leadUnitSearchOptions = getEndPointOptionsForLeadUnit();
    this.$coiList.next();
    this.setAdvanceSearch();
  }

  actionsOnPageChange(event) {
    this.reviewerDashboardService.reviewerRequestObject.currentPage = event;
    this.$coiList.next();
  }

  changeTab(tabName) {
    this.coiList = [];
    this.resetAdvanceSearchFields();
    this.reviewerDashboardService.reviewerRequestObject.tabName = tabName;
    this.setAdvanceSearch();
    this.$coiList.next();
  }

  setAdvanceSearch() {
    if (this.reviewerDashboardService.reviewerRequestObject.tabName === 'ALL_DISCLOSURES') {
      document.getElementById('collapseExample').classList.add('show');
      this.isShowAllProposalList = false;
    } else {
      document.getElementById('collapseExample').classList.remove('show');
      this.isShowAllProposalList = true;
    }
  }

  getCount() {
    this.$subscriptions.push(this.reviewerDashboardService.loadDisclosureReviewerQuickCardCounts().subscribe((data:any) => {
      console.log(data);
    }));
  }

  getDashboardDetails() {
    this.$subscriptions.push(this.$coiList.pipe(
      switchMap(() => this.reviewerDashboardService.getCOIReviewerDashboard(this.getRequestObject())))
      .subscribe((data: any) => {
        this.result = data || [];
        if (this.result) {
          this.coiList = this.result.disclosureViews || [];
          this.coiList.map(ele => {
            ele.numberOfProposals = ele.disclosureStatusCode != 1 ? ele.noOfProposalInActive : ele.noOfProposalInPending;
            ele.numberOfAwards = ele.disclosureStatusCode != 1 ? ele.noOfAwardInActive : ele.noOfAwardInPending;
          });
        }
        this.setEventTypeFlag();
      }));
  }

  getRequestObject() {
    this.setAdvanceSearchValuesToServiceObject();
    return this.reviewerDashboardService.reviewerRequestObject;
  }


  setAdvanceSearchValuesToServiceObject() {
    this.reviewerDashboardService.reviewerRequestObject.property6 = parseDateWithoutTimestamp(this.advanceSearchDates.certificationDate);
    this.reviewerDashboardService.reviewerRequestObject.property7 = parseDateWithoutTimestamp(this.advanceSearchDates.expirationDate);
  }

  redirectToDisclosure(coi: any) {
    this._router.navigate(['fibi/coi'], {queryParams: {disclosureId: coi.disclosure_id}});
  }

  selectPersonName(person: any) {
    this.reviewerDashboardService.reviewerRequestObject.property2 = person ? person.prncpl_id : null;
  }

  onLookupSelect(data: any, property: string) {
    this.lookupValues[property] = data;
    this.reviewerDashboardService.reviewerRequestObject[property] = data.length ? data.map(d => d.code) : [];
  }

  leadUnitChangeFunction(unit: any) {
    this.reviewerDashboardService.reviewerRequestObject.property3 = unit ? unit.unitNumber : null;
  }

  resetAndPerformAdvanceSearch() {
    this.resetAdvanceSearchFields();
    this.$coiList.next();
  }

  private resetAdvanceSearchFields() {
    this.reviewerDashboardService.reviewerRequestObject = new ReviewerDashboardRequest();
    this.advanceSearchDates = { certificationDate: null, expirationDate: null };
    this.advSearchClearField = new String('true');
    this.clearField = new String('true');
    this.lookupValues = [];
    this.sortCountObj = new SortCountObj();
    this.sortMap = {};
  }

  selectedEvent(event) {
    this.reviewerDashboardService.reviewerRequestObject.property8 = event ? event.coiEntityId : null;
  }

  performAdvanceSearch() {
    this.reviewerDashboardService.reviewerRequestObject.advancedSearch = 'A';
    this.reviewerDashboardService.reviewerRequestObject.currentPage = 1;
    this.isShowAllProposalList = true;
    this.$coiList.next();
  }

  isActive(colName) {
    if (!isEmptyObject(this.reviewerDashboardService.reviewerRequestObject.sort) && Object.keys(this.reviewerDashboardService.reviewerRequestObject.sort).includes(colName)) {
      return true;
    } else {
      return false;
    }
  }

  sortResult(sortFieldBy) {
    this.sortCountObj[sortFieldBy]++;
    if (this.sortCountObj[sortFieldBy] < 3) {
      this.sortMap[sortFieldBy] = !this.sortMap[sortFieldBy] ? 'asc' : 'desc';
    } else {
      this.sortCountObj[sortFieldBy] = 0;
      delete this.sortMap[sortFieldBy];
    }
    this.reviewerDashboardService.reviewerRequestObject.sort = this.sortMap;
    this.$coiList.next();
  }


  toggleADSearch() {
    if(document.getElementById('collapseExample').classList.contains('show')) {
      document.getElementById('collapseExample').classList.remove('show');
    } else {
      document.getElementById('collapseExample').classList.add('show');
    }
  }

  getReviewStatusBadge(statusCode) {
    switch (statusCode) {
      case '1':
        return 'warning';
      case '2':
        return 'info';
      case '3':
        return 'success';
      default:
        return 'danger';
    }
  }

  getDisclosureStatusBadge(statusCode) {
    switch (statusCode) {
      case 1:
        return 'warning';
      case 2:
      case 4:
      case 5:
        return 'info';
      case 3:
      case 6:
        return 'success';
      default:
        return 'danger';
    }
  }

  getDispositionStatusBadge(statusCode) {
    switch (statusCode) {
      case 1:
        return 'warning';
      case 2:
      case 3:
        return 'success';
      default:
        return 'info';
    }
  }

  getEventType(disclosureSequenceStatusCode, disclosureCategoryType) {
    if (disclosureCategoryType == 1) {
      if (disclosureSequenceStatusCode == 2 || disclosureSequenceStatusCode == 1 && !this.isActiveDisclosureAvailable) {
        return 'Active';
      } else if (disclosureSequenceStatusCode == 1 && this.isActiveDisclosureAvailable) {
        return 'Revision';
      }
    } else if (disclosureCategoryType == 3) {
      return 'Proposal';
    }
  }

  setEventTypeFlag() {
    this.isActiveDisclosureAvailable = !!this.coiList.find((ele: any) => ele.disclosureSequenceStatusCode == '2');
  }

}
