import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { TravelActionAfterSubmitRO, TravelConflictRO, TravelDisclosure, TravelHistoryRO } from '../travel-disclosure-interface';


@Injectable()
export class TravelDisclosureService {

    coiTravelDisclosure = new TravelDisclosure();
    saveSubject = new Subject();

    isAdminDashboard = false;
    travelDataChanged = false;
    isTravelCertified = false;
    isChildRouteTriggered = false;

    unSavedTabName = '';
    PREVIOUS_MODULE_URL = '';

    constructor(private _http: HttpClient,
                private _commonService: CommonService) { }

    setUnSavedChanges(dataChange: boolean, tabName: string): void {
        this.unSavedTabName = tabName;
        this.travelDataChanged = dataChange;
    }

    isCheckLoggedUser(personId: string): boolean {
        return personId === this._commonService.getCurrentUserDetail('personId');
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

    withdrawTravelDisclosure(travelDisclosureRO: TravelActionAfterSubmitRO) {
        return this._http.post(`${this._commonService.baseUrl}/withdrawTravelDisclosure`, travelDisclosureRO);
    }

    approveTravelDisclosure(travelDisclosureRO: TravelActionAfterSubmitRO) {
        return this._http.post(`${this._commonService.baseUrl}/approveTravelDisclosure`, travelDisclosureRO);
    }

    returnTravelDisclosure(travelDisclosureRO: TravelActionAfterSubmitRO) {
        return this._http.post(`${this._commonService.baseUrl}/returnTravelDisclosure`, travelDisclosureRO);
    }

    loadTravelDisclosureHistory(travelHistoryRO: TravelHistoryRO) {
        return this._http.post(`${this._commonService.baseUrl}/loadTravelDisclosureHistory`, travelHistoryRO);
    }

    getTravelConflictStatusType() {
        return this._http.get( `${this._commonService.baseUrl}/getTravelConflictStatusType`);
    }

    manageTravelConflict(params: TravelConflictRO) {
        return this._http.post(`${this._commonService.baseUrl}/manageTravelConflict`, params);
    }

    loadTravelConflictHistory(travelDisclosureId: number) {
        return this._http.get(`${this._commonService.baseUrl}/loadTravelConflictHistory/${travelDisclosureId}`);
    }
    getTravelDisclosureHistory(travelDisclosureId: number) {
        return this._http.get(`${this._commonService.baseUrl}/travelDisclosureHistory/${travelDisclosureId}`);
    }
}
