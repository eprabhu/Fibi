import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';

import { CommonService } from '../../common/services/common.service';

@Injectable()
export class CoiService {

    triggerAddReviewComment$: Subject<any> = new Subject();
    triggerReviewCommentDataUpdate$: Subject<any> = new Subject();
    globalSave$: Subject<any> = new Subject<any>();
    unSavedModules = '';

    isShowInfo = true;
    isShowSFIInfo = true;
    isShowCertifyInfo = true;
    isShowHistoryInfo = true;
    isShowAttachmentInfo = true;
    stepTabName = '';
    isCertified = false;

    constructor(
        private _http: HttpClient,
        private _commonService: CommonService
    ) { }

    loadDisclosure(disclosureId: string) {
        return this._http.get(`${this._commonService.baseUrl}/loadDisclosure/${disclosureId}`);
    }

    createDisclosure(params: any) {
        return this._http.post(`${this._commonService.baseUrl}/createDisclosure`, params);
    }

    certifyDisclosure(params: any) {
        return this._http.patch(this._commonService.baseUrl + '/certifyDisclosure', params);
    }

    evaluateDisclosureQuestionnaire(params: any) {
        return this._http.post(this._commonService.baseUrl + '/evaluateDisclosureQuestionnaire', params);
    }

    completeDisclosureReview(disclosureId: any, disclosureNumber: any) {
        return this._http.post(`${this._commonService.baseUrl}/completeDisclosureReview/${disclosureId}/${disclosureNumber}`, {});
    }

    triggerCommentModal(data: any) {
        this.triggerAddReviewComment$.next(data);
    }

    saveOrUpdateCoiReview(params: any) {
        return this._http.post(this._commonService.baseUrl + '/saveOrUpdateCoiReview', params);
    }

    getCoiProjectTypes() {
        return this._http.get(this._commonService.baseUrl + '/getCoiProjectTypes');
    }

    createCoiTravelDisclosure(obj: object) {
        return this._http.post(`${this._commonService.baseUrl}/createCoiTravelDisclosure`, obj);
    }

    loadTravelStatusTypesLookup() {
        return this._http.get(`${this._commonService.baseUrl}/loadTravelStatusTypesLookup`);
    }

    loadTravellerTypesLookup() {
        return this._http.get(`${this._commonService.baseUrl}/loadTravellerTypesLookup`);
    }

    getApplicableQuestionnaire(requestObject: any) {
        return this._http.post(`${this._commonService.fibiUrl}/getApplicableQuestionnaire`, requestObject);
    }

}
