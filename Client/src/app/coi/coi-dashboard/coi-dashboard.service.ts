import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Injectable()
export class CoiDashboardService {

constructor(private _http: HttpClient, private _commonService: CommonService) { }

    getCOIDashboard(param) {
        return this._http.post(this._commonService.baseUrl + '/getCOIDashboard', param );
    }

    reviseDisclosure(reviseObject) {
        return this._http.post(this._commonService.baseUrl + '/reviseDisclosure', reviseObject);
    }

    getSFIDashboard(param) {
        return this._http.post(this._commonService.baseUrl + '/getSFIDashboard', param );
    }

    loadProposalsForDisclosure(param) {
        return this._http.post(this._commonService.baseUrl + '/loadProposalsForDisclosure', param );
    }

    createDisclosure(params) {
        return this._http.post(this._commonService.baseUrl + '/createDisclosure', params);
    }

    getCOIHistory(disNum) {
        return this._http.post(this._commonService.baseUrl + '/loadDisclosureHistory', {'disclosureNumber' : disNum} );
    }
}

