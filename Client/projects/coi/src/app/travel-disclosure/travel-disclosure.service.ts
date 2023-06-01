import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';
import { CommonService } from '../common/services/common.service';

@Injectable()
export class TravelDisclosureService {

    travelDisclosureSubject = new Subject();
    travelDataChanged = false;
    isTravelCertified = false;
    unSavedTabName = '';

    constructor(
        private _http: HttpClient,
        private _commonService: CommonService
    ) { }

    createCoiTravelDisclosure(travelDisclosureRO: object) {
        return this._http.post(`${this._commonService.baseUrl}/createCoiTravelDisclosure`, travelDisclosureRO);
    }

    loadTravelStatusTypesLookup() {
        return this._http.get(`${this._commonService.baseUrl}/loadTravelStatusTypesLookup`);
    }

    loadTravellerTypesLookup() {
        return this._http.get(`${this._commonService.baseUrl}/loadTravellerTypesLookup`);
    }
}
