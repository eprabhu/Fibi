import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../common/services/common.service';
import { SFI } from './add-sfi.interface';

@Injectable()
export class AddSfiService {

    lookups: any;
    sfiDetails: SFI;
    previousURL= '';

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    addSFILookUp() {
        return this._http.get(this._commonService.baseUrl + '/loadSFILookups');
    }

    searchEntity(params) {
        return this._http.post(this._commonService.baseUrl + '/searchEntity', params);
    }

    createSfiDetails(params) {
        return this._http.post(this._commonService.baseUrl + '/createSFI', params);
    }

    getSFIDetails(coiFinancialEntityId: string) {
        return this._http.get(`${this._commonService.baseUrl}/getSFIDetails/${coiFinancialEntityId}`);
    }

    saveOrUpdateCoiFinancialEntityDetails(params) {
        return this._http.post(this._commonService.baseUrl + '/saveOrUpdateCoiFinancialEntityDetails', params);
    }

}
