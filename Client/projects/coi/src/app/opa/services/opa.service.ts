import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {CommonService} from '../../common/services/common.service';
import {of, Subject} from 'rxjs';
import { FormBuilderEvent } from '../../shared/form-builder-view/form-builder-interface';

@Injectable()
export class OpaService {
    previousHomeUrl = '';
    isReviewActionCompleted = false;
    isStartReview = false;
    isCompleteReview = false;
    isDisclosureReviewer = false;
    $SelectedReviewerDetails = new Subject();
    isShowCommentNavBar = false;
    isEnableReviewActionModal = false;
    isCOIAdministrator = false;
    formBuilderEvents = new Subject<FormBuilderEvent>();

    constructor(private _http: HttpClient,
                private _commonService: CommonService) {
    }

    loadOPA(disclosureId) {
        return this._http.get(this._commonService.baseUrl + '/opa/getOPADisclosureHeader/' + disclosureId);
    }

    getOPAReview(disclosureId) {
        return of({});
    }

    getApplicableQuestionnaire(disclosureId) {
        return of({});
    }

    isAllReviewsCompleted(flag) {
        return true;
    }

    triggerStartOrCompleteCoiReview(modalType) {

    }

    disclosureHistory(disclosureId) {
        return this._http.get(`${this._commonService.baseUrl}/opa/opaDisclosureHistory/${disclosureId}`);
    }
    submitOPA(opaDisclosureId, opaDisclosureNumber) {
        return this._http.patch(`${this._commonService.baseUrl}/opa/submit`, {opaDisclosureId, opaDisclosureNumber});
    }
    returnOPA(OPADisclosureID, OPADisclosureNumber) {
        return this._http.patch(`${this._commonService.baseUrl}/opa/return/${OPADisclosureID}/${OPADisclosureNumber}`, {});
    }
    withdrawOPA(OPADisclosureID, OPADisclosureNumber) {
        return this._http.patch(`${this._commonService.baseUrl}/opa/withdraw/${OPADisclosureID}/${OPADisclosureNumber}`, {});
    }

    completeOPAReview(OPADisclosureID, OPADisclosureNumber) {
        return this._http.patch(`${this._commonService.baseUrl}/opa/complete/${OPADisclosureID}/${OPADisclosureNumber}`, {});
    }

}
