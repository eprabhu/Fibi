import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { HttpClient } from '@angular/common/http';

@Injectable()
export class EntityComplianceService {

    entityCompliance: any;

    constructor(private _commonService: CommonService, private _http: HttpClient) { }

    fetchEntityComplianceDetails(entityId) {
        return this._http.get(`${this._commonService.baseUrl}/entity/compliance/fetch/${entityId}`);
    }

}
