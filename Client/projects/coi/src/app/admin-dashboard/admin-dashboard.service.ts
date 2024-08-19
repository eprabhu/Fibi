import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { of } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { CommonService } from '../common/services/common.service';
import { HTTP_ERROR_STATUS } from '../app-constants';
import { CoiProjectOverviewRequest, CoiDashboardRequest, NameObject, SortCountObj } from './admin-dashboard.interface';

@Injectable()
export class AdminDashboardService {

  coiRequestObject = new CoiDashboardRequest();
  projectOverviewRequestObject = new CoiProjectOverviewRequest();
  isAdvanceSearch: any;
  searchDefaultValues: NameObject = new NameObject();
  sortCountObject: SortCountObj = new SortCountObj();
  sort: any;
  constructor(private _http: HttpClient,
              private _commonService: CommonService) {
  }

  getCOIAdminDashboard(params: any) {
      return this._http.post(this._commonService.baseUrl + '/getCOIAdminDashboard', params).pipe(catchError((err) => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching COI Disclosure List failed. Please try again.');
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

  completeDisclosureReviews(disclosureNumberMap) {
    return this._http.patch(this._commonService.baseUrl + '/completeDisclosureReviews', disclosureNumberMap);
  }

  getLookupDataForProposalStatus(){
    return this._http.get(this._commonService.baseUrl + '/project/getProjectStatusLookup/Proposal');
  }

}