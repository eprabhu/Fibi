import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Injectable({
    providedIn: 'root'
})
export class EntitySponsorService {

    constructor(private _http: HttpClient, private _commonService: CommonService) { }


    saveSponsorRisk(sponsorRiskRO) {
        return this._http.post(this._commonService.baseUrl + '/entity/sponsor/saveRisk', sponsorRiskRO);
    }

    fetchRiskType() {
        return this._http.get(this._commonService.baseUrl + '/entity/fetchRiskTypes');
    }

    fetchEntityDetails(entityId) {
        return this._http.get(`${this._commonService.baseUrl}/entity/sponsor/fetch/${entityId}`);
    }

    deleteRisk(entityRiskId) {
        return this._http.delete(this._commonService.baseUrl + '/entity/sponsor/deleteRisk/'+ entityRiskId);
    }

    updateSponsorRisk(params: any) {
        return this._http.patch(this._commonService.baseUrl + '/entity/sponsor/updateRisk', params, { responseType: 'text' });
    }

    SponsorDetailsAutoSave(autoSaveRO){
        return this._http.post(this._commonService.baseUrl + '/entity/sponsor/save', autoSaveRO);
    }

    updateSponsorDetails(changedRO) {
        return this._http.patch(this._commonService.baseUrl + '/entity/sponsor/update', changedRO);
    }

}
