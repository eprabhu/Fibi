import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { of } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { HTTP_ERROR_STATUS } from '../../../../fibi/src/app/app-constants';
import { CommonService } from '../common/services/common.service';

@Injectable()
export class ReviewerDashboardService {

reviewerRequestObject = new ReviewerDashboardRequest();

constructor(private _http: HttpClient,
  private _commonService: CommonService) { }


  getCOIReviewerDashboard(params: any) {
  return this._http.post(this._commonService.baseUrl + '/getCOIReviewerDashboard', params).pipe(catchError((err) => {
    return of();
  }));
}

loadDisclosureReviewerQuickCardCounts() {
  return this._http.get(this._commonService.baseUrl + '/loadDisclosureReviewerQuickCardCounts');
}
}

export class ReviewerDashboardRequest {
  isDownload = false;
  property1 = '';
  property2 = null;
  property3 = null;
  property4 = null;
  property5 = null;
  property6 = null;
  property7 = null;
  property8 = null;
  property9 = null;
  pageNumber = 20;
  sort: any = {};
  tabName = '';
  advancedSearch = 'L';
  currentPage = 1;
  constructor(tabname?) {
    this.tabName = tabname ? tabname : 'NEW_SUBMISSIONS';
  }
}

export class SortCountObj {
   coiDisclosureNumber = 0;
   disclosurePersonFullName = 0;
   disclosureCategoryType = 0;
   disclosureStatus = 0;
   expirationDate = 0;
   updateTimeStamp = 0;
   certificationDate = 0;
}
