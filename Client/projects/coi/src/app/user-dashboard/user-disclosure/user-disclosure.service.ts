import {Injectable} from '@angular/core';
import {CommonService} from "../../common/services/common.service";
import {HttpClient} from "@angular/common/http";

@Injectable()
export class UserDisclosureService {

  constructor(private _commonService: CommonService, private _http: HttpClient) { }

  getCOIDashboard(param) {
    return this._http.post(this._commonService.baseUrl + '/getCOIDashboard', param );
  }
}
