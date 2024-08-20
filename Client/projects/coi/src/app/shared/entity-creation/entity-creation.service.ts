import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Injectable()
export class EntityCreationService {

  constructor(private _http: HttpClient,
    private _commonService: CommonService) { }

  autoSaveService(changedRO) {
    return this._http.patch(this._commonService.baseUrl + '/entity/update', changedRO);
  }

  createEntity(entityRO) {
    return this._http.post(this._commonService.baseUrl + '/entity/create', entityRO);
  }

  validateDUNS(dunsNumber) {
    return this._http.post(this._commonService.baseUrl + '/entity/dunsNumberExists', {dunsNumber: dunsNumber});
  }

  validateUEI(ueiNumber) {
    return this._http.post(this._commonService.baseUrl + '/entity/ueiNumberExists', {ueiNumber: ueiNumber});
  }

  validateCAGE(cageNumber) {
    return this._http.post(this._commonService.baseUrl + '/entity/cageNumberExists', {cageNumber: cageNumber});
  }

}
