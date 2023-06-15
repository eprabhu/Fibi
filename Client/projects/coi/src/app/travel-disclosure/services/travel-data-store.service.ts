import { Injectable } from '@angular/core';
import { TravelCreateModalDetails, TravelDisclosureResponseObject } from '../travel-disclosure-interface';
import { Subject } from 'rxjs';

@Injectable()
export class TravelDataStoreService {
    private storeData: TravelDisclosureResponseObject = new TravelDisclosureResponseObject();
    dataEvent = new Subject();
    travelCreateModalDetails: TravelCreateModalDetails;

    constructor() { }

    getCreateModalDetails(): TravelCreateModalDetails {
        this.travelCreateModalDetails = JSON.parse(sessionStorage.getItem('travelCreateModalDetails'));
        return this.travelCreateModalDetails;
    }

    removeCreateModalDetails(): void {
        sessionStorage.removeItem('travelCreateModalDetails');
    }

    getData(keys?: Array<string>): TravelDisclosureResponseObject {
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

    setStoreData(data: TravelDisclosureResponseObject): void {
        this.storeData = this.structuredClone(data);
    }

    private structuredClone(obj: TravelDisclosureResponseObject): any {
        const nativeCloneFunction = (window as any).structuredClone;
        return (typeof nativeCloneFunction === 'function') ? nativeCloneFunction(obj) : JSON.parse(JSON.stringify(obj));
    }

    getEditModeForDisclosure(): boolean {
        if (this.storeData.travelDisclosureId) {
            return this.storeData.travelDisclosureStatusCode === '1';
        } else {
            return true;
        }
    }
}
