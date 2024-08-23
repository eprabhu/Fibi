import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { FCOIDisclosureCreateRO } from '../shared-interface';

@Injectable()
export class DisclosureCreateModalService {

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    reviseDisclosure(reviseObject) {
        return this._http.post(this._commonService.baseUrl + '/reviseDisclosure', reviseObject);
    }

    createDisclosure(params: FCOIDisclosureCreateRO) {
        return this._http.post(`${this._commonService.baseUrl}/fcoiDisclosure`, params);
    }

    getCoiProjectTypes() {
        return this._http.get(this._commonService.baseUrl + '/getCoiProjectTypes');
    }

    checkIfDisclosureAvailable(moduleCode: any, moduleItemKey: any) {
        return this._http.post(this._commonService.baseUrl + '/fcoiDisclosure/validate', {
            moduleCode: moduleCode,
            moduleItemKey: moduleItemKey,
            personId: this._commonService.getCurrentUserDetail('personID')
        });
    }

}
