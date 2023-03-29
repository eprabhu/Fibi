import { Injectable } from '@angular/core';
import { BehaviorSubject, Subject } from 'rxjs';
import { CommentConfiguration } from '../coi-interface';
import { CoiService } from '../services/coi.service';

@Injectable()
export class CoiSummaryEventsAndStoreService {

    $isToolkitVisible = new BehaviorSubject(true);
    $projectDetails: BehaviorSubject<any> = new BehaviorSubject<any>(null);
    dataEvent = new Subject();
    concatenatedProjectList: any = [];
    conflictStatusList: any = [];

    coiSummaryConfig: any = {
        currentDisclosureId: null,
    };

    private storeData: any = {};

    constructor(private _coiService: CoiService) { }

    getData(disclosureId: string, keys?: Array<string>): any {
        if (!keys) {
            return this.structuredClone(this.storeData[disclosureId]);
        }
        const data: any = {};
        keys.forEach(key => {
            data[key] = this.storeData[disclosureId][key];
        });
        return this.structuredClone(data);
    }

    setStoreData(data: any, key: string): void {
        this.storeData[key] = this.structuredClone(data);
    }

    manualDataUpdate(updatedData): void {
        const KEYS = Object.keys(updatedData);
        KEYS.forEach(key => {
            this.storeData[key] = this.structuredClone(updatedData[key]);
        });
        this.dataEvent.next(KEYS);
    }

    private structuredClone(obj: any): any {
        const nativeCloneFunction = (window as any).structuredClone;
        return (typeof nativeCloneFunction === 'function') ? nativeCloneFunction(obj) : JSON.parse(JSON.stringify(obj));
    }

    modifyReviewComment(config: CommentConfiguration) {
        this._coiService.triggerCommentModal(config);
    }

}
