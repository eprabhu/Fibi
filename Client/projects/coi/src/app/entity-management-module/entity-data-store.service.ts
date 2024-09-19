import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';
import { EntireEntityDetails, EntityDetails, EntityTabStatus } from './shared/entity-interface';
import { CommonService } from '../common/services/common.service';
import { ENTITY_DOCUMNET_STATUS_TYPE, ENTITY_VERIFICATION_STATUS } from '../app-constants';

@Injectable()

export class EntityDataStoreService {
    constructor(private _commonService: CommonService) { }

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
        if(type === 'ORG' || type === 'BOTH') {
            entityTabStatus.organization_feed_status_code = '2';
            entityTabStatus.organization_feed_status = 'Ready to Feed';
        }
        if(type === 'SPONSOR' || type === 'BOTH') {
            entityTabStatus.sponsor_feed_status_code = '2';
            entityTabStatus.sponsor_feed_status = 'Ready to Feed';
        }
        this.updateStore(['entityTabStatus'], { entityTabStatus });
    }

    enableModificationHistoryTracking() {
        this.canLogModificationHistory = this.storeData?.entityDetails?.entityStatusType?.entityStatusTypeCode === ENTITY_VERIFICATION_STATUS.VERIFIED;
    }

}
