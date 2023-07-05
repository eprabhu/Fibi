import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { TravelDisclosureResponseObject, TravelHistoryRO } from '../travel-disclosure-interface';


@Injectable()
export class TravelDisclosureService {

    coiTravelDisclosure = new TravelDisclosureResponseObject();
    saveSubject = new Subject();
    travelDataChanged = false;
    isTravelCertified = false;
    unSavedTabName = '';
    modalActionBtnName = '';
    isChildRouting = false;
    isAdminDashboard = false;
    PREVIOUS_MODULE_URL = '';

    constructor(private _http: HttpClient,
                private _commonService: CommonService) { }

    setUnSavedChanges(dataChange: boolean, tabName: string): void {
        this.unSavedTabName = tabName;
        this.travelDataChanged = dataChange;
    }

    createCoiTravelDisclosure(travelDisclosureRO: object) {
        return this._http.post(`${this._commonService.baseUrl}/createCoiTravelDisclosure`, travelDisclosureRO);
    }

    submitTravelDisclosure(travelDisclosureRO: object) {
        return this._http.post(`${this._commonService.baseUrl}/submitTravelDisclosure`, travelDisclosureRO);
    }
    certifyTravelDisclosure(travelDisclosureRO: object) {
        return this._http.post(`${this._commonService.baseUrl}/certifyTravelDisclosure`, travelDisclosureRO);
    }

    loadTravelStatusTypesLookup() {
        return this._http.get(`${this._commonService.baseUrl}/loadTravelStatusTypesLookup`);
    }

    loadTravellerTypesLookup() {
        return this._http.get(`${this._commonService.baseUrl}/loadTravellerTypesLookup`);
    }

    loadTravelDisclosure(travelDisclosureId: number) {
        return this._http.get(`${this._commonService.baseUrl}/loadTravelDisclosure/${travelDisclosureId}`);
    }

    withdrawTravelDisclosure(travelDisclosureRO: object) {
        return this._http.post(`${this._commonService.baseUrl}/withdrawTravelDisclosure`, travelDisclosureRO);
    }

    approveTravelDisclosure(travelDisclosureRO: object) {
        return this._http.post(`${this._commonService.baseUrl}/approveTravelDisclosure`, travelDisclosureRO);
    }

    returnTravelDisclosure(travelDisclosureRO: object) {
        return this._http.post(`${this._commonService.baseUrl}/returnTravelDisclosure`, travelDisclosureRO);
    }

    loadTravelDisclosureHistory(travelHistoryRO: TravelHistoryRO) {
        return this._http.post(`${this._commonService.baseUrl}/loadTravelDisclosureHistory`, travelHistoryRO);
    }
}
