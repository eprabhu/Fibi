import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../../app/common/services/common.service';
import { BehaviorSubject } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class EntityDetailsService {

  previousURL = '';
  lookups:any;
  $entityDetails = new BehaviorSubject<object>({});
  isExpanded = true;
  constructor(private _http: HttpClient, private _commonService: CommonService) { }

  getSFIDetails(coiFinancialEntityId) {
    return this._http.get(`${this._commonService.baseUrl}/getSFIDetails/${coiFinancialEntityId}`);
  }

  saveOrUpdateCoiFinancialEntityDetails(params) {
    return this._http.post(this._commonService.baseUrl + '/saveOrUpdateCoiFinancialEntityDetails', params);
  }

  addSFILookUp() {
    return this._http.get(this._commonService.baseUrl + '/loadSFILookups');
  }

}
