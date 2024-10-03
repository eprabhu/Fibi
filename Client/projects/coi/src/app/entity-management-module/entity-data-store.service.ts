import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';
import { EntireEntityDetails, EntityDetails, EntityTabStatus, SubAwardOrgUpdateClass } from './shared/entity-interface';
import { CommonService } from '../common/services/common.service';
import { ENTITY_DOCUMNET_STATUS_TYPE, ENTITY_VERIFICATION_STATUS, FEED_STATUS_CODE } from '../app-constants';
import { canUpdateSponsorFeed, canUpdateOrgFeed } from './entity-management.service';
import { HttpClient } from '@angular/common/http';

@Injectable()

export class EntityDataStoreService {
    constructor(private _commonService: CommonService, private _http: HttpClient) { }

    private storeData = new EntireEntityDetails();
    entitySectionConfig: any = {};
    entityRiskType: any[] = [];
    dataEvent = new Subject<string[]>();
    canLogModificationHistory = false;

    getEditMode(): boolean {
        return this.storeData?.entityDetails?.entityDocumentStatusType?.documentStatusTypeCode === ENTITY_DOCUMNET_STATUS_TYPE.ACTIVE &&
               (this._commonService.isEntityModified || this.storeData?.entityDetails?.entityStatusType?.entityStatusTypeCode == ENTITY_VERIFICATION_STATUS.UNVERIFIED );
    }

    // All Mandatory fields(Entity Name,Ownership Type, Addres 1, City,State,Country,Zip/postal code)
    getIsEntityMandatoryFilled() {
        if (this.storeData.entityDetails) {
            const {
                entityName, entityOwnershipTypeCode, primaryAddressLine1,
                city, state, country, postCode
            } = this.storeData.entityDetails;
            return !!entityName && !!entityOwnershipTypeCode && !!primaryAddressLine1 && !!city && !!state && !!country && !!postCode;
        }
        return false;
    }

    getData(keys?: Array<string>): any {
        if (!keys) {
            return this.structuredClone(this.storeData);
        }
        const data: any = {};
        keys.forEach(key => {
            data[key] = this.storeData[key];
        });
        return this.structuredClone(data);
    }

    updateStore(updatedData: string[], variable): void {
        const UPDATED_DATA = {};
        updatedData.forEach(element => {
            UPDATED_DATA[element] = variable[element];
        });
        this.manualDataUpdate(UPDATED_DATA);
    }

    manualDataUpdate(updatedData: any): void {
        const KEYS = Object.keys(updatedData);
        KEYS.forEach(key => {
            this.storeData[key] = this.structuredClone(updatedData[key]);
        });
        this.dataEvent.next(KEYS);
    }

    setStoreData(data: any): void {
        this.storeData = this.structuredClone(data);
        const KEYS = Object.keys(this.storeData);
        this.dataEvent.next(KEYS);
    }

    getFilterRiskByCode(code: 'EN' | 'SP' | 'OR' | 'CO' | ''): any[] {
        return this.entityRiskType.length ? this.entityRiskType.filter(ele => ele.riskCategoryCode == code) : this.entityRiskType;
    }

    private structuredClone(obj: any): any {
        const nativeCloneFunction = (window as any).structuredClone;
        return (typeof nativeCloneFunction === 'function') ? nativeCloneFunction(obj) : JSON.parse(JSON.stringify(obj));
    }

    updateModifiedFlag(entityDetails: EntityDetails, isModified: boolean) {
        this._commonService.isEntityModified = isModified;
        this.updateStore(['entityDetails'], { entityDetails });
    }

    updateFeedStatus(entityTabStatus: EntityTabStatus, type: 'ORG'|'SPONSOR'|'BOTH') {
        if((type === 'ORG' || type === 'BOTH') && this.storeData?.entityTabStatus?.entity_sub_org_info) {
            entityTabStatus.organization_feed_status_code = FEED_STATUS_CODE.READY_TO_FEED;
            entityTabStatus.organization_feed_status = 'Ready to Feed';
        }
        if((type === 'SPONSOR' || type === 'BOTH') && this.storeData?.entityTabStatus?.entity_sponsor_info) {
            entityTabStatus.sponsor_feed_status_code = FEED_STATUS_CODE.READY_TO_FEED;
            entityTabStatus.sponsor_feed_status = 'Ready to Feed';
        }
        this.updateStore(['entityTabStatus'], { entityTabStatus });
    }

    enableModificationHistoryTracking() {
        // this.canLogModificationHistory = this.storeData?.entityDetails?.entityStatusType?.entityStatusTypeCode === ENTITY_VERIFICATION_STATUS.VERIFIED;
    }

    getApiCalls(entityId, reqObj): any[] {
        const REQUEST = [];
        const REQ_OBJ = { entityId: entityId, feedStatusCode: FEED_STATUS_CODE.READY_TO_FEED }
        if(canUpdateSponsorFeed(reqObj) && this.storeData?.entityTabStatus?.entity_sponsor_info) {
            REQUEST.push(this._http.patch(`${this._commonService.baseUrl}/entity/sponsor/update`, REQ_OBJ));
        }
        const SUBAWARD_REQ_OBJ: SubAwardOrgUpdateClass = { entityId, subAwardOrgFields: { feedStatusCode: FEED_STATUS_CODE.READY_TO_FEED } };
        if(canUpdateOrgFeed(reqObj) && this.storeData?.entityTabStatus?.entity_sub_org_info) {
            REQUEST.push(this._http.patch(`${this._commonService.baseUrl}/entity/organization/update`, SUBAWARD_REQ_OBJ));
        }
        return REQUEST;
    }

}
