import { Injectable } from '@angular/core';

import { Subject } from 'rxjs';
import { COI } from '../coi-interface';

@Injectable()
export class DataStoreService {

    constructor() { }

    private storeData: COI = new COI();
    currentDashboardTab = 'CURRENT_DISCLOSURES';

    entityDetails = [
        // tslint:disable-next-line: max-line-length
        {entityId: 1, entityName: 'Samsung', country: 'Japan', email: 'samsung@samsung.com', phNum: '1800 456 45464', status: 'Active', address: 'Suwon, Gyeonggi-do', zip: '108-8240', url: 'https://www.samsung.com/global', createDate: '01/08/2021', createdBy: 'Adu-Djan, Abhinav K', approvedBy: 'Will Smith', noOfSFI: '11', state: 'Tokyo' , isShowApproved: false},
        // tslint:disable-next-line: max-line-length
        {entityId: 2, entityName: 'Google', country: 'USA', email: 'google@gmail.com', phNum: '1800 456 2912', status: 'Active', address: '4311 Washington Avenue, Jackson, Mississippi', zip: '456595353', url: 'https://www.google.com', createDate: '01/10/2021', createdBy: 'Daniel Griffith', approvedBy: 'Will Smith', noOfSFI: '07', state: 'Mississippi' , isShowApproved: true},
        {entityId: 3, entityName: 'Embry-Riddle Aeronautical University', country: 'USA', email: 'embry@gmail.com', phNum: '1800 456 45464', status: 'Active', address: '3890 Michael Street,  Sugar Land, Alaska', zip: '108-8240', url: 'https://www.embry.com/global', createDate: '09/08/2021', createdBy: 'Adu-Djan, Abhinav K', approvedBy: 'Will Smith', noOfSFI: '02', state: 'Alaska' , isShowApproved: false},
        // tslint:disable-next-line: max-line-length
        {entityId: 4, entityName: 'Medrobotics', country: 'USA', email: 'medrobotics@medrobotics.com', phNum: '1800 456 45464', status: 'InActive', address: '30 Mulberry Street, Center, Alaska', zip: '115086926104', url: 'https://www.medrobotics.com', createDate: '01/08/2021', createdBy: 'Daniel Griffith', approvedBy: 'Adu-Djan, Abhinav K', noOfSFI: '01', state: 'Alaska' , isShowApproved: false},
        {entityId: 5, entityName: 'ProMab Biotechnologies', country: 'USA', email: 'promabBioTechnologies@promab.com', phNum: '1800 456 45464', status: 'Active', address: '4311 Washington Avenue, Jackson, Alaska', zip: '94804-5836', url: 'https://www.promab.com', createDate: '01/08/2021', createdBy: 'Daniel Griffith', approvedBy: 'Will Smith', noOfSFI: '10', state: 'Alaska' , isShowApproved: false},
        // tslint:disable-next-line: max-line-length
        {entityId: 6, entityName: 'J. D. Edwards', country: 'USA', email: 'edward@jdedwards.com', phNum: '1801 916 42464', status: 'Active', address: '4311 Washington Avenue, Jackson, Badakhshan', zip: '80237', url: 'https://www.jdedwards.com', createDate: '02/11/2021', createdBy: 'Adu-Djan, Abhinav K', approvedBy: 'Will Smith', noOfSFI: '03', state: 'Badakhshan' , isShowApproved: true},
        {entityId: 7, entityName: 'Greenwall Foundation', country: 'USA', email: 'greenwall@gmail.com', phNum: '1800 456 40032', status: 'InActive', address: '4311 Washington Avenue, Jackson, Badakhshan', zip: '10170', url: 'https://www.greenwall.com', createDate: '03/08/2021', createdBy: 'Will Smith', approvedBy: 'Daniel Griffith', noOfSFI: '05', state: 'Badakhshan' , isShowApproved: false},
        // tslint:disable-next-line: max-line-length
        {entityId: 8, entityName: 'Millennium Challenge Corporation', country: 'USA', email: 'millennium@ch.com', phNum: '1800 143 45464', status: 'Active', address: '30 Mulberry Street, Center, Colorado', zip: '20005', url: 'https://www.millennium.com', createDate: '05/10/2021', createdBy: 'Adu-Djan, Abhinav K', approvedBy: 'Will Smith', noOfSFI: '02', state: 'Colorado' , isShowApproved: false}
    ];

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
        return this.storeData.coiDisclosure.conflictStatusCode === '1';
    }

}
