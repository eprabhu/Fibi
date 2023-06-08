import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { BehaviorSubject, Subject } from 'rxjs';

import { CommonService } from '../../common/services/common.service';

@Injectable()
export class CoiService {

    triggerAddReviewComment$: Subject<any> = new Subject();
    triggerReviewCommentDataUpdate$: Subject<any> = new Subject();
    globalSave$: Subject<any> = new Subject<any>();
    unSavedModules = '';
    previousHomeUrl = '';
    isShowInfo = true;
    isShowSFIInfo = true;
    isShowCertifyInfo = true;
    isShowHistoryInfo = true;
    isShowAttachmentInfo = true;
    stepTabName = '';
    isCertified = false;
    isReviewActionCompleted = false;
    $reviewList = new BehaviorSubject<any>([]);
    isCOIAdministrator = false;

    constructor(
        private _http: HttpClient,
        private _commonService: CommonService
    ) { }

    loadDisclosure(disclosureId: string) {
        return this._http.get(`${this._commonService.baseUrl}/loadDisclosure/${disclosureId}`);
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

    getApplicableQuestionnaire(requestObject: any) {
        return this._http.post(`${this._commonService.fibiUrl}/getApplicableQuestionnaire`, requestObject);
    }

    getAdminDetails() {
      return this._http.get(this._commonService.baseUrl + '/adminGroup/adminPersons');
    }

    getPersonGroup (personId) {
      return this._http.get(this._commonService.baseUrl + '/getPersonGroup', {
        headers: new HttpHeaders().set('personId', personId.toString())
      });
    }

    assignAdmin(params) {
        return this._http.patch(this._commonService.baseUrl + '/disclosure/assignAdmin', params);
    }

    getCoiReview(disclosureId: number) {
      return this._http.get(`${this._commonService.baseUrl}/getCoiReview/${disclosureId}`);
  }
}
