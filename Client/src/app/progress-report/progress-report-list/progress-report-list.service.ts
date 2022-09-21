import { Injectable } from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {CommonService} from '../../common/services/common.service';
import { Subject } from 'rxjs';

@Injectable()
export class ProgressReportListService {
  sortCountObject: any = {};
  httpOptions: any = {};
  dashboardRequestObject: any = {
    property1: '',
    property2: '',
    property3: '',
    property4: '',
    property5: '',
    property6: '',
    property7: '',
    property8: [],
    property9: '',
    property10: '',
    property11: [],
    property12: '',
    property13: '',
    pageNumber: 20,
    sort: {},
    tabName: 'PENDING_PR',
    advancedSearch: 'L',
    sortBy: '',
    currentPage: 1
  };
  /** For storing default Value of end point or elastic search */
  dashboardRequestExtraData: any = {
    unitName: '',
    fullName: '',
    isEmployeeFlag: true
  };

  constructor(private _http: HttpClient, private _commonService: CommonService) { }

  getProgressReportDashBoardList(params) {
    return this._http.post(this._commonService.baseUrl + '/fibiProgressReportDashBoard', params);
  }

  setHttpOptions(contextField, formatString, path, defaultValue, params) {
    this.httpOptions.contextField = contextField;
    this.httpOptions.formatString = formatString;
    this.httpOptions.path = path;
    this.httpOptions.defaultValue = defaultValue;
    this.httpOptions.params = params;
    return JSON.parse(JSON.stringify(this.httpOptions));
  }
}
