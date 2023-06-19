import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Injectable()
export class AssignAdministratorModalService {

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    getAdminDetails() {
        return this._http.get(this._commonService.baseUrl + '/adminGroup/adminPersons');
    }

    getPersonGroup(personId) {
        return this._http.get(this._commonService.fibiUrl + '/getPersonGroup', {
            headers: new HttpHeaders().set('personId', personId.toString())
        });
    }

    assignAdmin(params) {
        return this._http.patch(this._commonService.baseUrl + '/disclosure/assignAdmin', params);
    }
    
}
