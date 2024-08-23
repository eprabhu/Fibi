import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {CommonService} from '../../common/services/common.service';
import { RO } from '../../disclosure/coi-interface';
import { URL_FOR_DISCLOSURE_PROJECT } from '../../app-constants';

@Injectable()
export class CountModalService {

    constructor(private _http: HttpClient,
                private _commonService: CommonService) {
    }

    getSFICount(params: RO) {
        return this._http.post(this._commonService.baseUrl + '/personEntity/fetch', params);
    }

    getDisclosureProjects(disclosureId: number) {
        return this._http.get(this._commonService.baseUrl + URL_FOR_DISCLOSURE_PROJECT.replace('{disclosureId}', disclosureId.toString()));
    }

    getDisclosureDetails(id) {
        return this._http.get(`${this._commonService.baseUrl}/getDisclosureDetailsForSFI/${id}`);
    }
}
