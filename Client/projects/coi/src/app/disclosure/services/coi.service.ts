import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';

import { CommonService } from '../../common/services/common.service';

@Injectable()
export class CoiService {

    triggerAddReviewComment$: Subject<any> = new Subject();
    triggerReviewCommentDataUpdate$: Subject<any> = new Subject();

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

    completeDisclosureReview(disclosureId: any) {
        return this._http.post(`${this._commonService.baseUrl}/completeDisclosureReview/${disclosureId}`, {});
    }

    triggerCommentModal(data: any) {
        this.triggerAddReviewComment$.next(data);
    }


}
