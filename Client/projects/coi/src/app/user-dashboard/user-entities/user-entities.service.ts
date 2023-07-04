import { Injectable } from '@angular/core';
import {HttpClient} from "@angular/common/http";
import {CommonService} from "../../common/services/common.service";
import { GetSFIRequestObject } from '../../disclosure/coi-interface';

@Injectable()
export class UserEntitiesService {

  constructor(private _http: HttpClient,
              private _commonService: CommonService) { }


  getSFIDashboard(param: GetSFIRequestObject) {
    return this._http.post(this._commonService.baseUrl + '/getSFIOfDisclosure', param);
  }
}
