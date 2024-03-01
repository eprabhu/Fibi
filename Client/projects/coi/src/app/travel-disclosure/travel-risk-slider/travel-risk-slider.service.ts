import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Injectable()

export class TravelRiskSliderService {

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    getTravelRiskLookup() {
        return this._http.get(this._commonService.baseUrl + '/disclosure/risk');
    }

    saveRisk(params) {
        return this._http.put(this._commonService.baseUrl + '/travelDisclosure/modifyRisk', params);
    }

    getTravelRiskHistory(params) {
        return this._http.post(this._commonService.baseUrl + '/travelDisclosure/history', params);
    }

    riskAlreadyModified(params: any) {
        return this._http.post(`${this._commonService.baseUrl}/travelDisclosure/riskStatus`, params);
     }

}

