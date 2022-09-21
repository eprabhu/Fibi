import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { CommonService } from '../../common/services/common.service';

@Injectable()
export class OverviewService {
  constructor(private _commonService: CommonService, private _http: HttpClient) {
  }

  saveOrUpdateInstProposal(data): Observable<any> {
    return this._http.post(this._commonService.baseUrl + '/saveOrUpdateInstituteProposal', data);
  }

  fetchHelpText(param) {
    return this._http.post(this._commonService.baseUrl + '/fetchHelpTexts', param);
}

}
