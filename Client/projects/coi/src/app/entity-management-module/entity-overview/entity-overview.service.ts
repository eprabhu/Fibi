import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Injectable()
export class EntityOverviewService {

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    addAdditionalAddress(additionalDetailsRO) {
        return this._http.post(this._commonService.entityURL + '/saveAdditionalAddresses', additionalDetailsRO);
    }
    addRegistrationDetails(registrationDetails) {
        return this._http.post(this._commonService.entityURL + '/saveRegistrationDetails', registrationDetails);
    }

    updateRegistrationDetails(registrationDetails) {
        return this._http.post(this._commonService.entityURL + '/saveRegistrationDetails', registrationDetails);
    }

    updateOtherDetails(otherDetailsRO) {
        console.log(otherDetailsRO);
        // return this._http.post(this._commonService.entityURL + '/saveRegistrationDetails', registrationDetails);
    }
}
