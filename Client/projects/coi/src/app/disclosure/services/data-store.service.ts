import { Injectable } from '@angular/core';

import { Subject } from 'rxjs';
import { COI } from '../coi-interface';

@Injectable()
export class DataStoreService {

    constructor() { }

    private storeData: COI = new COI();
    disclosureStatus: any;
    dataChanged = false;
    disclosureSectionConfig: any = {};

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

    setStoreData(data: COI): void {
        this.storeData = this.structuredClone(data);
    }

    private structuredClone(obj: any): any {
        const nativeCloneFunction = (window as any).structuredClone;
        return (typeof nativeCloneFunction === 'function') ? nativeCloneFunction(obj) : JSON.parse(JSON.stringify(obj));
    }

    getEditModeForCOI(): boolean {
        if (this.storeData.coiDisclosure) {
            return this.storeData.coiDisclosure.dispositionStatusCode === '1';
        } else {
            return false;
        }
    }

}
