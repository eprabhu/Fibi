import { Injectable } from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {CommonService} from '../services/common.service';

@Injectable()
export class HeaderService {

  constructor(private _http: HttpClient,
              private _commonService: CommonService) { }

    createOPA(personId, homeUnit) {
        return this._http.post(this._commonService.baseUrl + '/opa/createOPA', {personId, homeUnit});
    }

}
