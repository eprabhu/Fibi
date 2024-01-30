import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';
import {OpaDisclosure} from '../opa-interface';

export class StoreData {
    opaDisclosure = new OpaDisclosure();
}

@Injectable()
export class DataStoreService {

    constructor() { }

    private storeData: StoreData = new StoreData();
    disclosureStatus: any;
    dataChanged = false;

    dataEvent = new Subject();
    updateTimestampEvent  = new Subject();

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

    getEditModeForOPA(): boolean {
        if (this.storeData.opaDisclosure.opaDisclosureId) {
          return ['1', '5', '6'].includes(this.storeData.opaDisclosure.reviewStatusType.reviewStatusCode);
        } else {
            return false;
        }
    }

}
