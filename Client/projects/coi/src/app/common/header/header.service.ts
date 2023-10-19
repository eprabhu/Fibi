import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../services/common.service';

@Injectable()

export class HeaderService {

    constructor(private _http: HttpClient,
        private _commonService: CommonService) { }


    saveOrUpdatePersonNote(req: any) {
        return this._http.post(this._commonService.baseUrl + '/saveOrUpdatePersonNote', req);
    }

    createOPA(personId, homeUnit) {
        return this._http.post(this._commonService.opaUrl + '/createOPA', { personId, homeUnit });
    }

}
