import { Injectable } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { Subject } from 'rxjs';
import { CONSULTING_REVIEW_STATUS } from '../../app-constants';
import { ConsultingFormDisclosure } from '../consulting-form.interface';

export class StoreData {
    consultingFormDisclosure = new ConsultingFormDisclosure();
}

@Injectable()
export class DataStoreService {

    constructor(private _commonService: CommonService) { }

    private storeData: StoreData = new StoreData();
    disclosureStatus: any;
    dataChanged = false;
    opaDisclosureSectionConfig : any = {};

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
        if (this.storeData.consultingFormDisclosure.disclosureId) {
          return [CONSULTING_REVIEW_STATUS.PENDING, CONSULTING_REVIEW_STATUS.RETURNED, CONSULTING_REVIEW_STATUS.WITHDRAWN]
              .includes(this.storeData.consultingFormDisclosure.reviewStatusType && this.storeData.consultingFormDisclosure.reviewStatusType.reviewStatusCode)
          && this.isLoggedInUser(this.storeData.consultingFormDisclosure.person.personId);
        } else {
            return false;
        }
    }

    isLoggedInUser(personId: string) {
        return this._commonService?.getCurrentUserDetail('personID') === personId;
    }

}
