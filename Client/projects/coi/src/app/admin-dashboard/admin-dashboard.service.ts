import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { of } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { CommonService } from '../common/services/common.service';

@Injectable()
export class AdminDashboardService {

  coiRequestObject = new CoiDashboardRequest();
  isAdvanceSearch: any;
  searchDefaultValues: NameObject = new NameObject();
  sortCountObject: SortCountObj = new SortCountObj();
  sort: any;

  constructor(private _http: HttpClient,
              private _commonService: CommonService) {
  }

  getCOIAdminDashboard(params: any) {
      return this._http.post(this._commonService.baseUrl + '/getCOIAdminDashboard', params).pipe(catchError((err) => {
        return of();
      }));
  }

  startCOIReview(coiReviewId: number) {
      return this._http.post(`${this._commonService.baseUrl}/startCOIReview`,
          {coiReview: {coiReviewId}});
  }

  completeCOIReview(coiReviewId: number) {
      return this._http.post(`${this._commonService.baseUrl}/completeCOIReview`,
          {coiReview: {coiReviewId}});
  }

  loadCoiReviewComments(req: any) {
      return this._http.post(this._commonService.baseUrl + '/loadCoiReviewComments', req);
  }

  addCOIReviewComment(params: any) {
      const formData = new FormData();
      formData.append('formDataJson', JSON.stringify(params));
      return this._http.post(this._commonService.baseUrl + '/addCOIReviewComment', formData);
  }

}

export class CoiDashboardRequest {
  isDownload = false;
  property1 = null;
  property2 = null;
  property3 = null;
  property4 = [];
  property5 = [];
  property6 = null;
  property7 = null;
  property8 = null;
  property9 = null;
  property10 = null;
  property11 = null;
  property12 = null;
  property13 = null;
  property14 = null;
  property15 = null;
  property20 = [];
  property21 = [];
  property22 = null;
  property23 = null;
  pageNumber = 20;
  sort: any = {'updateTimeStamp': 'desc'};
  tabName = '';
  advancedSearch = 'L';
  currentPage = 1;
  constructor(tabName?) {
    this.tabName = tabName ? tabName : 'MY_REVIEWS';
  }
}

export class SortCountObj {
  coiDisclosureNumber = 0;
  disclosurePersonFullName = 0;
  disclosureCategoryType = 0;
  disclosureStatus = 0;
  dispositionStatus = 0;
  reviewStatus = 0;
  expirationDate = 0;
  certificationDate = 0;
  travellerName = 0;
  travelEntityName = 0;
  travelState = 0;
  travelCountry = 0;
  travelCity = 0;
  documentStatusDescription = 0;
  reviewDescription = 0;
  certifiedAt = 0;
  travelExpirationDate = 0;
  updateTimeStamp = 2;
}

export class NameObject {
  entityName = '';
  personName = '';
  departmentName = '';
}
