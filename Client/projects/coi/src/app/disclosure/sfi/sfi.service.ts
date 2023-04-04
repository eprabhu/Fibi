import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../common/services/common.service';
import { SFI } from './add-sfi.interface';

@Injectable({
    providedIn: 'root'
})
export class SfiService {
    isShowSfiNavBar = false;
    sfiDetails: SFI;
    previousURL= '';
    
    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    getSfiDetails(id, disclosureStatusCode, personId) {
        return this._http.post(this._commonService.baseUrl + '/getSFIOfDisclosure',
            {'disclosureId': id, 'disclosureStatusCode': disclosureStatusCode, 'personId': personId});
    }

    createSFI(prams) {
    return this._http.post(this._commonService.baseUrl + '/createSFI', prams)
  }

  getSFIDetails(coiFinancialEntityId) {
    return this._http.get(`${this._commonService.baseUrl}/getSFIDetails/${coiFinancialEntityId}`)
  }

}
