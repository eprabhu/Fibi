import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Injectable()
export class EntitySponsorService {

    entitySponsorDetails: any = {};


    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    
    fetchEntitySponsorDetails(entityId) {
        return this._http.get(`${this._commonService.baseUrl}/entity/sponsor/fetch/${entityId}`);
    }

    saveSponsorRisk(sponsorRiskRO: any) {
        return this._http.post(this._commonService.baseUrl + '/entity/sponsor/saveRisk', sponsorRiskRO);
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
