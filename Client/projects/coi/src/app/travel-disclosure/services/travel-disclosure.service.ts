import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { EntityDetails, TravelActionAfterSubmitRO, TravelConflictRO, TravelDisclosure, TravelHistoryRO } from '../travel-disclosure.interface';

@Injectable()
export class TravelDisclosureService {

    saveSubject = new Subject();
    travelEntityDetails = new EntityDetails();
    coiTravelDisclosure = new TravelDisclosure();

    isAdminDashboard = false;
    travelDataChanged = false;
    isAllowNavigation = false;
    isTravelCertified = false;
    isCreateNewTravelDisclosure = false;

    unSavedTabName = '';
    PREVIOUS_MODULE_URL = '';
    concurrentUpdateAction = '';


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

    riskAlreadyModified(params: any) {
        return this._http.post(`${this._commonService.baseUrl}/travelDisclosure/riskStatus`, params);
    }
}
