import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';
import { CommonService } from '../common/services/common.service';

@Injectable()
export class TravelDisclosureService {

    public travelDisclosureSubject = new Subject();

    constructor(
        private _http: HttpClient,
        private _commonService: CommonService
    ) { }
    
    createCoiTravelDisclosure(obj: object) {
        return this._http.post(`${this._commonService.baseUrl}/createCoiTravelDisclosure`, obj);
    }

    loadTravelStatusTypesLookup() {
        return this._http.get(`${this._commonService.baseUrl}/loadTravelStatusTypesLookup`);
    }

    loadTravellerTypesLookup() {
        return this._http.get(`${this._commonService.baseUrl}/loadTravellerTypesLookup`);
    }
}
