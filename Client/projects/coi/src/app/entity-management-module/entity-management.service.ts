import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../common/services/common.service';
import { Subject, Subscription } from 'rxjs';
import { EntityDataStoreService } from './entity-data-store.service';

@Injectable()
export class EntityManagementService {

    hasChangesAvailable: boolean;
    $subscriptions: Subscription[] = [];
    triggerDUNSEntity = new Subject();

    constructor(private _commonService: CommonService, private _http: HttpClient,
        private _dataStore: EntityDataStoreService
    ) { }

    getEntityDetails(entityId) {
        return this._http.get(`${this._commonService.baseUrl}/entity/fetch/${entityId}`);
    }

    fetchRiskType() {
        return this._http.get(this._commonService.baseUrl + '/entity/fetchRiskTypes');
    }

    getDunsMatch(test) {
        return this._http.post(this._commonService.fibiCOIConnectUrl + '/fibi-coi-connect/cleansematch/entity/runCleanseMatch', test);
    }

}


