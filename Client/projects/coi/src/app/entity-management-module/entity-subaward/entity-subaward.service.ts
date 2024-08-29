import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { SubAwardOrganization } from '../shared/entity-interface';

@Injectable()
export class EntitySubAwardService {

    entitySubAwardOrganization = new SubAwardOrganization();

    constructor(private _http: HttpClient, private _commonService: CommonService) { }

    fetchEntityOrganizationDetails(entityId: string | number) {
        return this._http.get(`${this._commonService.baseUrl}/entity/organization/fetch/${entityId}`);
    }

    organizationDetailsAutoSave(autoSaveRO){
        return this._http.post(`${this._commonService.baseUrl}/entity/organization/save`, autoSaveRO);
    }

    updateOrganizationDetails(changedRO) {
        return this._http.patch(`${this._commonService.baseUrl}/entity/organization/update`, changedRO);
    }

}
