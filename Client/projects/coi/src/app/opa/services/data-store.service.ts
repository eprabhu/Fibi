import {Injectable} from '@angular/core';
import {Subject} from 'rxjs';
import {OpaDisclosure} from '../opa-interface';
import {OPA_REVIEW_STATUS} from '../../app-constants';
import {CommonService} from '../../common/services/common.service';

export class StoreData {
    opaDisclosure = new OpaDisclosure();
}

@Injectable()
export class DataStoreService {

    constructor(private _commonService: CommonService) { }

    private storeData: StoreData = new StoreData();
    disclosureStatus: any;
    dataChanged = false;

    dataEvent = new Subject();

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

    setStoreData(data): void {
        this.storeData = this.structuredClone(data);
    }

    private structuredClone(obj: any): any {
        const nativeCloneFunction = (window as any).structuredClone;
        return (typeof nativeCloneFunction === 'function') ? nativeCloneFunction(obj) : JSON.parse(JSON.stringify(obj));
    }

    isFormEditable(): boolean {
        if (this.storeData.opaDisclosure.opaDisclosureId) {
          return [OPA_REVIEW_STATUS.PENDING, OPA_REVIEW_STATUS.RETURNED, OPA_REVIEW_STATUS.WITHDRAWN]
              .includes(this.storeData.opaDisclosure.reviewStatusType.reviewStatusCode)
          && this.isLoggedInUser(this.storeData.opaDisclosure.personId);
        } else {
            return false;
        }
    }

    isLoggedInUser(personId: string) {
        return this._commonService?.getCurrentUserDetail('personId') === personId;
    }
}
