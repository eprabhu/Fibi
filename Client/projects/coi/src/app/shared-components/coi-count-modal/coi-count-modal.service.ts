import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { URL_FOR_DISCLOSURE_PROJECT } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';
import { RO } from '../review-comments-slider/review-comments-interface';

@Injectable()
export class CoiCountModalService {

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    getSFICount(params: RO) {
        return this._http.post(`${this._commonService.baseUrl}/personEntity/fetch`, params);
    }
    
    getDisclosureDetails(disclosureId: number) {
        return this._http.get(`${this._commonService.baseUrl}/getDisclosureDetailsForSFI/${disclosureId}`);
    }

    getDisclosureProjects(disclosureId: number) {
        return this._http.get(`${this._commonService.baseUrl}${URL_FOR_DISCLOSURE_PROJECT.replace('{disclosureId}', disclosureId.toString())}`);
    }

}
