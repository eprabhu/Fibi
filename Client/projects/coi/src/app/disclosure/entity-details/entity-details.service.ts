import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../../app/common/services/common.service';
import { BehaviorSubject } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class EntityDetailsService {

  previousURL = '';
  lookups: any;
  entityDetails:any;
  $entityDetailsTest = new BehaviorSubject<object>({});

  $relationshipsDetails = new BehaviorSubject<object>({});
  isExpanded = true;
  constructor(private _http: HttpClient, private _commonService: CommonService) { }

  getSFIDetails(coiFinancialEntityId) {
    return this._http.get(`${this._commonService.baseUrl}/getSFIDetails/${coiFinancialEntityId}`);
  }

  saveOrUpdateCoiFinancialEntityDetails(params) {
    return this._http.post(this._commonService.baseUrl + '/saveOrUpdateCoiFinancialEntityDetails', params);
  }

  addSFILookUp(tabName) {
    return this._http.get(`${this._commonService.baseUrl}/getRelationshipLookup/${tabName}`);
  }

  getCoiEntityDetails(personEntityId) {
    return this._http.get(`${this._commonService.baseUrl}/getCoiEntityDetails/${personEntityId}`);
  }

  getRelationshipEntityDetails(personEntityId) {
    return this._http.get(`${this._commonService.baseUrl}/getPersonEntityDetails/${personEntityId}`);
  }

  getPersonEntityRelationship(params){
    return this._http.post(this._commonService.baseUrl + '/getPersonEntityRelationship',params)
  }

}
