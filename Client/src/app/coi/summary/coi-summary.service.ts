import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';

import { CommonService } from '../../common/services/common.service';

@Injectable()
export class CoiSummaryService {

    constructor(
        private _http: HttpClient,
        private _commonService: CommonService
    ) { }

    getProjectRelationships(params: any) {
        return this._http.post(`${this._commonService.baseUrl}/getDisclosureRelations`, params);
    }

    getEntityProjectRelations(params: any) {
        return this._http.post(this._commonService.baseUrl + '/getEntityProjectRelations', params);
    }

    getSfiDetails(params: any) {
        return this._http.post(this._commonService.baseUrl + '/getSFIOfDisclosure', params);
    }

    updateProjectConflictStatus(params: any) {
        return this._http.post(this._commonService.baseUrl + '/updateProjectConflictStatus', params);
    }

    loadProjectConflictHistory(disclosureDetailsId: any) {
        return this._http.get(`${this._commonService.baseUrl}/loadProjectConflictHistory/${disclosureDetailsId}`);
    }

}
