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

    deletePriorName(id) {
        return this._http.delete(this._commonService.baseUrl + '/entity/deletePriorName/'+ id);
    }

    fetchIndustryCategoryCode(categoryTypeCode) {
        return this._http.get(`${this._commonService.baseUrl}/entity/fetchIndustryCategoryCode/${categoryTypeCode}`);
    }

    updateIndustryDetails(industryObj) {
        return this._http.post(`${this._commonService.baseUrl}/entity/updateIndustryDetails}`, industryObj);
    }
    deleteIndustryDetails(entityIndustryClassId) {
        return this._http.delete(`${this._commonService.baseUrl}/entity/deleteIndustryDetails/${entityIndustryClassId}}`);
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

    updateRisk(riskRO) {
        return this._http.patch(this._commonService.baseUrl + '/entity/updateRisk', riskRO);
    }
    saveExternalReference(externalRO) {
        return this._http.post(this._commonService.baseUrl + '/entity/saveExternalReference', externalRO);
    }
    updateExternalReference(externalRO) {
        return this._http.patch(this._commonService.baseUrl + '/entity/updateExternalReference', externalRO);
    }
    deleteExternalReference(entityExternalMappingId) {
        return this._http.delete(this._commonService.baseUrl + '/entity/deleteExternalReference/' + entityExternalMappingId);
    }

    fetchRiskType() {
        return this._http.get(this._commonService.baseUrl + '/entity/fetchRiskTypes/EN');
    }

    updateOtherDetails(otherDetailsRO) {
        return this._http.patch(this._commonService.baseUrl + '/entity/updateOtherDetails', otherDetailsRO);
    }
}
