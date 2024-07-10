import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Injectable()
export class AssignAdministratorModalService {

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    getAdminDetails(moduleCode) {
        return this._http.get(this._commonService.baseUrl + '/adminGroup/adminPersons/' + moduleCode);
    }

    getPersonGroup(personId) {
        return this._http.get(this._commonService.fibiUrl + '/getPersonGroup', {
            headers: new HttpHeaders().set('personId', personId.toString())
        });
    }

    assignAdmin(path: string, params) {
        const url = path === 'opa' ? this._commonService.formUrl + '/opa/assignAdmin' :
          path === 'consultingDisclosure' ? this._commonService.baseUrl + '/consultingDisclosure/assignAdmin' : this._commonService.baseUrl + `/${path}/assignAdmin`;
        return this._http.patch(url, params);
    }

}
