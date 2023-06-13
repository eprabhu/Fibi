import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { TravelDisclosureResponseObject } from '../travel-disclosure-interface';


@Injectable()
export class TravelDisclosureService {

    coiTravelDisclosure = new TravelDisclosureResponseObject();
    saveOrCopySubject = new Subject();
    travelDataChanged = false;
    isTravelCertified = false;
    unSavedTabName = '';
    isChildRouting = false;

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    setUnSavedChanges(dataChange: boolean, tabName: string): void {
        this.unSavedTabName = tabName;
        this.travelDataChanged = dataChange;
    }

    createCoiTravelDisclosure(travelDisclosureRO: object) {
        return this._http.post(`${this._commonService.baseUrl}/createCoiTravelDisclosure`, travelDisclosureRO);
    }

    loadTravelStatusTypesLookup() {
        return this._http.get(`${this._commonService.baseUrl}/loadTravelStatusTypesLookup`);
    }

    loadTravellerTypesLookup() {
        return this._http.get(`${this._commonService.baseUrl}/loadTravellerTypesLookup`);
    }

    loadTravelDisclosure(travelDisclosureId: string) {
        return this._http.get(`${this._commonService.baseUrl}/loadTravelDisclosure/${travelDisclosureId}`);
    }
}
