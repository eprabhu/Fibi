import { Injectable } from '@angular/core';
import {HttpClient} from "@angular/common/http";
import {CommonService} from "../../common/services/common.service";
import { RO } from '../../disclosure/coi-interface';
import { catchError } from 'rxjs/operators';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { of } from 'rxjs';

@Injectable()
export class UserEntitiesService {

  constructor(private _http: HttpClient,
              private _commonService: CommonService) { }


  getSFIDashboard(param: RO) {
    return this._http.post(this._commonService.baseUrl + '/getSFIOfDisclosure', param).pipe(catchError((err) => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching Proposal list failed. Please try again.');
        return of();
    }));
  }
}
