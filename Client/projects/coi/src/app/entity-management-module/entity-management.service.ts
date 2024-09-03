import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../common/services/common.service';
import { Subject, Subscription } from 'rxjs';
import { jumpToSection } from '../common/utilities/custom-utilities';

@Injectable()
export class EntityManagementService {

    hasChangesAvailable: boolean;
    $subscriptions: Subscription[] = [];
    triggerDUNSEntity = new Subject();
    selectedSectionId = '';

    constructor(private _commonService: CommonService, private _http: HttpClient) { }

    scrollToSelectedSection(sectionId: string) {
        this.selectedSectionId = sectionId;
        const offset = (document.getElementById('COI-DISCLOSURE-HEADER')?.getBoundingClientRect().height + 100);
        jumpToSection(this.selectedSectionId, offset);
    }

    getEntityDetails(entityId) {
        return this._http.get(`${this._commonService.baseUrl}/entity/fetch/${entityId}`);
    }

    getDunsMatch(test) {
        return this._http.post(this._commonService.fibiCOIConnectUrl + '/fibi-coi-connect/cleansematch/entity/runCleanseMatch', test);
    }

    verifyEntity(entityId: string | number) {
        return this._http.patch(`${this._commonService.baseUrl}/entity/verify/${entityId}`, {});
    }

    fetchEntitySponsorDetails(entityId) {
        return this._http.get(`${this._commonService.baseUrl}/entity/sponsor/fetch/${entityId}`);
    }

    fetchEntityOrganizationDetails(entityId: string | number) {
        return this._http.get(`${this._commonService.baseUrl}/entity/organization/fetch/${entityId}`);
    }

}


