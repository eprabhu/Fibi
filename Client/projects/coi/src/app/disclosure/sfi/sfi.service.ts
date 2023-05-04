import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../common/services/common.service';
// import { SFI } from './add-sfi.interface';
import { Subject } from 'rxjs';

@Injectable()
export class SfiService {
    isShowSfiNavBar = false;
    isSFIRequired = false;
    previousURL= '';
    $addSfi = new Subject<Boolean>();

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    getSfiDetails(id, reviewStatusCode, personId) {
        return this._http.post(this._commonService.baseUrl + '/getSFIOfDisclosure',
            {'disclosureId': id, 'reviewStatus': reviewStatusCode, 'personId': personId});
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

    addSFILookUp() {
        return this._http.get(this._commonService.baseUrl + '/loadSFILookups');
    }

    saveOrUpdateCoiFinancialEntityDetails(params) {
        return this._http.post(this._commonService.baseUrl + '/saveOrUpdateCoiFinancialEntityDetails', params);
    }
    
}
