import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { of } from 'rxjs';
import { catchError } from 'rxjs/operators';
import { CommonService } from '../common/services/common.service';

@Injectable()
export class OpaDashboardService {

isAdvanceSearch = false;
sort: any;
opaRequestObject: OPADashboardRequest = new OPADashboardRequest();
sortCountObject: SortCountObj = new SortCountObj();
searchDefaultValues: NameObject = new NameObject();
isCheckBoxSelected = {
  'F': false,
  'S': false
};

constructor(private _http: HttpClient,
  private _commonService: CommonService) { }


getOPADashboard(params: any) {
  return this._http.post(this._commonService.formUrl + '/opa/dashboard', params).pipe(catchError((err) => {
    return of();
  }));
}

}

export class OPADashboardRequest {
  tabType = "MY_REVIEWS";
  filterType = null;
  pageNumber = 20;
  currentPage = 1;
  sort: any = {'updateTimeStamp': 'desc'};
  unitNumber= null;
  submissionTimestamp = null;
  dispositionStatusCodes = [];
  opaDisclosureStatusCodes = [];
  personId = null;
  isFaculty = null;
  periodStartDate = null;
  periodEndDate = null;
  constructor(tabName?) {
    this.tabType = tabName ? tabName : 'MY_REVIEWS';
  }
}

export class SortCountObj {
  createTimestamp = 0;
  person = 0;
  submissionTimestamp = 0;
  updateTimeStamp = 2;
  dispositionStatus = 0;
  disclosureStatus = 0;
  closeDate = 0;
}

export class NameObject {
  departmentName = '';
  personName = '';
}