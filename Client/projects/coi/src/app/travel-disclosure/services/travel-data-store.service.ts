import { Injectable } from '@angular/core';
import { CoiTravelDisclosure, TravelCreateModalDetails, TravelDisclosureResponseObject } from '../travel-disclosure-interface';
import { Subject } from 'rxjs';
import { convertToValidAmount } from 'projects/fibi/src/app/common/utilities/custom-utilities';
import { getTotalNoOfDays, parseDateWithoutTimestamp } from 'projects/fibi/src/app/common/utilities/date-utilities';

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

    getTravelDisclosureRO(): CoiTravelDisclosure {
        const data = this.getData();
        const travellerTypeCode = data.travellerTypeCodeList ? Object.keys(data.travellerTypeCodeList) : [];
        return {
            'entityId': data.entityId,
            'entityNumber': data.entityNumber,
            'travellerTypeCode': travellerTypeCode,
            'travelTitle': data.travelTitle,
            'isInternationalTravel': data.isInterNationalTravel,
            'travelState': data.travelState,
            'destinationCountry': data.destinationCountry,
            'destinationCity': data.destinationCity,
            'purposeOfTheTrip': data.purposeOfTheTrip,
            'relationshipToYourResearch': data.relationshipToYourResearch,
            'travelAmount': !data.travelAmount ? null : convertToValidAmount(data.travelAmount),
            'travelStartDate': parseDateWithoutTimestamp(data.travelStartDate),
            'travelEndDate': parseDateWithoutTimestamp(data.travelEndDate),
            'travelDisclosureId': data.travelDisclosureId,
            'homeUnit': data.homeUnitNumber,
            'description': data.description,
            'personId': data.personId,
            'isSponsoredTravel': true,
            'noOfDays': getTotalNoOfDays(data.travelStartDate, data.travelEndDate)
        };
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
            return ['1', '4', '5'].includes(this.storeData.reviewStatusCode);
        } else {
            return true;
        }
    }
}
