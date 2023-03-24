import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../../common/services/common.service';

@Injectable()
export class AreaOfResearchService {

constructor(private _http: HttpClient, private _commonService: CommonService) { }

deleteGrantCallAreaOfResearch(params) {
    return this._http.post(this._commonService.baseUrl + '/deleteGrantCallAreaOfResearch', params);
  }
addGrantCallAreaOfResearch(params) {
    return this._http.post(this._commonService.baseUrl + '/addGrantCallAreaOfResearch', params);
  }
}
