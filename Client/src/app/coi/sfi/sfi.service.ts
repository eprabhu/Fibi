import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { CommonService } from '../../common/services/common.service';

@Injectable({
  providedIn: 'root'
})
export class SfiService {

constructor(private _http: HttpClient, private _commonService: CommonService) { }

getSfiDetails(id, disclosureStatusCode, personId) {
  return this._http.post(this._commonService.baseUrl + '/getSFIOfDisclosure',
  {'disclosureId': id, 'disclosureStatusCode': disclosureStatusCode, 'personId': personId});
}

}
