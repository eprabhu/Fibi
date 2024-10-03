import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { CommonService } from '../common/services/common.service';
import { Subject, Subscription } from 'rxjs';
import { isEmptyObject, jumpToSection } from '../common/utilities/custom-utilities';
import { ORGANISATION_FIELDS, SPONSOR_FIELDS, SubawardOrgFields } from './shared/entity-interface';

@Injectable()
export class EntityManagementService {

    hasChangesAvailable: boolean;
    $subscriptions: Subscription[] = [];
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

    getDunsMatch(cleanseRequest) {
        return this._http.post(this._commonService.fibiCOIConnectUrl + '/fibi-coi-connect/cleansematch/entity/runCleanseMatch', cleanseRequest);
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

    updateIsDUNSMatchFlag(changedRO) {
        return this._http.patch(this._commonService.baseUrl + '/entity/update', changedRO);
    }

    logFeedHistory(tabDetails) {
        return this._http.post(this._commonService.baseUrl + '/entity/logAction', tabDetails);
    }

    triggerEnrichAPI(enrichRequest) {
        return this._http.post(this._commonService.fibiCOIConnectUrl + '/fibi-coi-connect/enrich/entity/runEnrich', enrichRequest);
    }

    entityHistory(entityId) {
        return this._http.get(`${this._commonService.baseUrl}/entity/fetchHistory/${entityId}`);
    }

}

export function getEntityFullAddress(entityDetails, valuesToAdd = ['primaryAddressLine1', 'primaryAddressLine2', 'city', 'state' ]): string {
    let address = '';
    Object.keys(entityDetails).forEach((ele: string) => {
        if(valuesToAdd.includes(ele) && entityDetails[ele]) {
            address = address + (address ? ' , ' : '') + entityDetails[ele];
        }
    })
    if (entityDetails?.country?.countryName) {
        address = address + (address ? ' , ' : '') + entityDetails?.country?.countryName;
    }
    return address;
}

export function canUpdateSponsorFeed(reqObj) {
    if(reqObj && !isEmptyObject(reqObj)) {
        return Object.keys(reqObj).some(key => SPONSOR_FIELDS.includes(key));
    } else {
        return false;
    }
}

export function canUpdateOrgFeed(reqObj: SubawardOrgFields) {
    if(reqObj && !isEmptyObject(reqObj)) {
    return Object.keys(reqObj).some(key => ORGANISATION_FIELDS.includes(key));
    } else {
        return false;
    }
}

