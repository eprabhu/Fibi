import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { HttpClient, HttpResponse } from '@angular/common/http';
import { Observable } from 'rxjs';

@Injectable()

export class FormService {

  constructor(private _http: HttpClient, private _commonService: CommonService) { }


  addSFILookUp() {
    return this._http.get(this._commonService.baseUrl + '/loadSFILookups');
  }

  isEntityAdded(entityId): Observable<HttpResponse<any>> {
    return this._http.get(`${this._commonService.baseUrl}/entity/isLinked/${entityId}/personEntity`, { observe: 'response' });
  }

  saveOrUpdateCoiFinancialEntityDetails(params) {
    return this._http.post(this._commonService.baseUrl + '/personEntity/addRelationship', params);
  }

  saveOrUpdateCoiEntity(params) {
    return this._http.post(this._commonService.baseUrl + '/saveOrUpdateCoiEntity', params)
  }

  savePersonEntity(params) {
    return this._http.post(this._commonService.baseUrl + '/personEntity', params);
  }

  saveFormPersonEntityDetails(params) {
    return this._http.patch(this._commonService.baseUrl + '/consultingDisclosure/savePersonEntityDetails', params);
  }

}
