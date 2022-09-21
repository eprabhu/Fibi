import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {CommonService} from '../../common/services/common.service';
import {Router} from '@angular/router';

@Injectable({
    providedIn: 'root'
})
export class ClaimOverviewService {

    constructor(private _http: HttpClient, private _commonService: CommonService, private _router: Router) {
    }

    saveOrUpdateClaim(params) {
        return this._http.post(this._commonService.baseUrl + '/saveOrUpdateClaim', params);
    }

}
