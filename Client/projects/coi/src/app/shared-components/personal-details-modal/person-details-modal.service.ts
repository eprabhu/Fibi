import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Injectable()
export class PersonDetailsModalService {

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    getPersonData(personId) {
        return this._http.post(this._commonService.fibiUrl + '/getPersonDetailById', { 'personId': personId });
    }

    loadPersonTrainingList(params) {
        return this._http.post(this._commonService.fibiUrl + '/getTrainingDashboard', params);
    }

}
