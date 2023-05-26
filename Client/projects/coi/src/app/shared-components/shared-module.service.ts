import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../common/services/common.service';

@Injectable()
export class SharedModuleService {

constructor(private _http: HttpClient, private _commonService: CommonService) { }

reviseDisclosure(reviseObject) {
  return this._http.post(this._commonService.baseUrl + '/reviseDisclosure', reviseObject);
}

createDisclosure(params: any) {
  return this._http.post(`${this._commonService.baseUrl}/createDisclosure`, params);
}

getCoiProjectTypes() {
  return this._http.get(this._commonService.baseUrl + '/getCoiProjectTypes');
}

checkIfDisclosureAvailable(moduleCode, moduleId) {
  return this._http.get(`${this._commonService.baseUrl}/validate/${moduleCode}/disclosure/${moduleId}`);
}

}
