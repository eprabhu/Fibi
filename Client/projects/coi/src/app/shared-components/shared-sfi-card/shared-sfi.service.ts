import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Injectable()
export class SharedSfiService {

  constructor(private _http: HttpClient, private _commonService: CommonService) { }

  deleteSFI(params) {
    return this._http.delete(`${this._commonService.baseUrl}/personEntity/${params}`);
  }

  modifySfi(params) {
    return this._http.post(this._commonService.baseUrl + '/personEntity/modify', params);
  }
}
