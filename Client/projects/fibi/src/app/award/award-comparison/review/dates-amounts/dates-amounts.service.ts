import { Injectable } from '@angular/core';
import {HttpClient } from '@angular/common/http';
import { CommonService } from '../../../../common/services/common.service';

@Injectable()
export class DatesAmountsService {

  constructor(private _http: HttpClient, private _commonService: CommonService) { }

datesAmountsLookUpData(param) {
    return this._http.post(this._commonService.baseUrl + '/getAwardDatesAndAmount', param);
  }
}
