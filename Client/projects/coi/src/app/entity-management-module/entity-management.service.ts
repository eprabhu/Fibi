import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../common/services/common.service';

@Injectable()
export class EntityManagementService {

constructor(private _commonService: CommonService, private _http: HttpClient) { }

getEntityDetails(entityId) {
    return this._http.get(`${this._commonService.entityURL}/fetch/${entityId}`);
}

}
