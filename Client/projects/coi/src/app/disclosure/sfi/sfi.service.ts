import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../common/services/common.service';
// import { SFI } from './add-sfi.interface';
import { Subject } from 'rxjs';
import { GetSFIRequestObject } from '../coi-interface';

@Injectable()
export class SfiService {
    isShowSfiNavBar = false;
    isSFIRequired = false;
    previousURL= '';
    $addSfi = new Subject<Boolean>();

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    getSfiDetails(params: GetSFIRequestObject) {
        return this._http.post(this._commonService.baseUrl + '/getSFIOfDisclosure', params);
    }

    deleteSFI(params) {
        return this._http.delete(`${this._commonService.baseUrl}/personEntity/${params}`);
    }

    createSFI(params) {
        return this._http.post(this._commonService.baseUrl + '/createSFI', params)
    }

    saveOrUpdateCoiEntity(params) {
        return this._http.post(this._commonService.baseUrl + '/saveOrUpdateCoiEntity', params)
    }

    getSFIDetails(coiFinancialEntityId) {
        return this._http.get(`${this._commonService.baseUrl}/getSFIDetails/${coiFinancialEntityId}`)
    }

    isEntityAdded(entityId) {
        return this._http.get(`${this._commonService.baseUrl}/checkEntity/${entityId}/added`)
    }

    addSFILookUp() {
        return this._http.get(this._commonService.baseUrl + '/loadSFILookups');
    }

    saveOrUpdateCoiFinancialEntityDetails(params) {
        return this._http.post(this._commonService.baseUrl + '/saveOrUpdateCoiFinancialEntityDetails', params);
    }
    getEntityDetails(entityId) {
      return this._http.get(`${this._commonService.baseUrl}/getEntityDetails/${entityId}`);
    }
}
