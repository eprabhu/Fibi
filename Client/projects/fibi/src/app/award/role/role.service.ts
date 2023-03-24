import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { HttpClient } from '@angular/common/http';

@Injectable()
export class RoleService {

  constructor(private _http: HttpClient, private _commonService: CommonService) { }

  fetchAwardPersonRoles(requestData) {
    return this._http.post(this._commonService.baseUrl + '/fetchAwardPersonRoles', requestData);
  }
  addAwardPersonRoles(requestData) {
    return this._http.post(this._commonService.baseUrl + '/addAwardPersonRoles', requestData);
  }
  deleteAwardPersonRoles(requestData) {
    return this._http.post(this._commonService.baseUrl + '/deleteAwardPersonRoles', requestData);
  }
}
