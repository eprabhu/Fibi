import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../services/common.service';
import {ActiveDisclosure} from "../../user-dashboard/user-disclosure/user-disclosure-interface";
import {Subject} from "rxjs";
import { FcoiType } from '../services/coi-common.interface';

@Injectable()

export class HeaderService {

    activeDisclosures: ActiveDisclosure[] = [];
    activeOPAs = [];
    $openModal = new Subject<FcoiType>();

    constructor(private _http: HttpClient,
        private _commonService: CommonService) { }

    getActiveDisclosure() {
        return this._http.get(this._commonService.baseUrl + '/getActiveDisclosures');
    }

    saveOrUpdatePersonNote(req: any) {
        return this._http.post(this._commonService.baseUrl + '/saveOrUpdatePersonNote', req);
    }

    createOPA(personId, homeUnit) {
        return this._http.post(this._commonService.opaUrl + '/createOPA', { personId, homeUnit });
    }

    createConsultingForm(personId, homeUnit) {
        return this._http.post(this._commonService.baseUrl + '/consultingDisclosure/create', { personId, homeUnit });
    }

}
