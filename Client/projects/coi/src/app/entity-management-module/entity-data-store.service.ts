import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';
import { EntireEntityDetails } from './shared/entity-interface';

@Injectable()

export class EntityDataStoreService {
    constructor() { }

    private storeData = new EntireEntityDetails();
    entitySectionConfig: any = {};
    entityRiskType: any[] = [];
    dataEvent = new Subject<string[]>();

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


}
