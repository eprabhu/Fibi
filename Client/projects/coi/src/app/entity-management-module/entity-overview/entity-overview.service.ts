import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';

@Injectable()
export class EntityOverviewService {

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    addAdditionalAddress(additionalDetailsRO) {
        return this._http.post(this._commonService.baseUrl + '/entity/saveAdditionalAddresses', additionalDetailsRO);
    }
    updateAdditionalAddresses(additionalDetailsRO) {
        return this._http.patch(this._commonService.baseUrl + '/entity/updateAdditionalAddresses', additionalDetailsRO);
    }
    deleteAdditionalAddress(id) {
        return this._http.delete(this._commonService.baseUrl + '/entity/deleteAdditionalAddress/' + id);
    }
    addRegistrationDetails(registrationDetails) {
        return this._http.post(this._commonService.baseUrl + '/entity/saveRegistrationDetails', registrationDetails);
    }
    updateRegistrationDetails(registrationDetails) {
        return this._http.patch(this._commonService.baseUrl + '/entity/updateRegistrationDetails', registrationDetails);
    }
    deleteRegistrationDetails(entityRegistrationId) {
        return this._http.delete(this._commonService.baseUrl + '/entity/deleteRegistrationDetails/'+ entityRegistrationId);
    }

    deleteRisk(id) {
        return this._http.delete(this._commonService.baseUrl + '/entity/deleteRisk/'+ id);
    }

    deleteForeignName(id) {
        return this._http.delete(this._commonService.baseUrl + '/entity/deleteForeignName/'+ id);
    }

    fetchIndustryCategoryCode(categoryTypeCode) {
        return this._http.get(`${this._commonService.baseUrl}/entity/fetchIndustryCategoryCode/${categoryTypeCode}`);
    }

    saveIndustryDetails(industryObj) {
        return this._http.post(this._commonService.baseUrl + '/entity/saveIndustryDetails', industryObj);
    }

    updatePrioirNameDetails(priorNameRO) {
        return this._http.post(this._commonService.baseUrl + '/entity/addPriorName', priorNameRO);
    }

    updateAlternateNameDetails(alternateNamesRO) {
        return this._http.post(this._commonService.baseUrl + '/entity/addForeignName', alternateNamesRO);
    }

    fetchCurrencyDetails() {
        return this._http.get(this._commonService.baseUrl + '/entity/fetchCurrencyDetails');
    }
    saveRisk(riskRO) {
        return this._http.post(this._commonService.baseUrl + '/entity/saveRisk', riskRO);
    }

    fetchRiskType() {
        return this._http.get(this._commonService.baseUrl + '/entity/fetchRiskTypes');
    }

    updateOtherDetails(otherDetailsRO) {
        return this._http.patch(this._commonService.baseUrl + '/entity/updateOtherDetails', otherDetailsRO);
    }
}
