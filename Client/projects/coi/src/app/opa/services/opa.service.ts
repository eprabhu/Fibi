import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {CommonService} from '../../common/services/common.service';
import {BehaviorSubject, of, Subject} from 'rxjs';
import { FormBuilderEvent } from '../../configuration/form-builder-create/shared/form-builder-view/form-builder-interface';

@Injectable()
export class OpaService {
    previousHomeUrl = '';
    isReviewActionCompleted = false;
    isStartReview = false;
    isCompleteReview = false;
    isDisclosureReviewer = false;
    $SelectedReviewerDetails = new BehaviorSubject({});
    isShowCommentNavBar = false;
    isEnableReviewActionModal = false;
    formBuilderEvents = new Subject<FormBuilderEvent>();
    actionButtonId = null;
    currentOPAReviewForAction: any;
    concurrentUpdateAction = '';
    isFormBuilderDataChangePresent = false;
    triggerSaveComplete = new Subject<boolean>()

    constructor(private _http: HttpClient,
                private _commonService: CommonService) {
    }

    loadOPA(disclosureId) {
        return this._http.get(this._commonService.opaUrl + '/getOPADisclosureHeader/' + disclosureId);
    }

    getOPAReview(disclosureId) {
        return this._http.get(this._commonService.opaUrl + '/review/' + disclosureId);
    }

    getApplicableQuestionnaire(disclosureId) {
        return of({});
    }

    isAllReviewsCompleted(reviewerList) {
        return reviewerList.every(value => value.reviewerStatusType && value.reviewerStatusType.reviewStatusCode === '2');
    }

    triggerStartOrCompleteCoiReview(modalType) {
        this.actionButtonId = modalType === 'START' ? 'opa-start-reviewer-review-modal-trigger' : 'opa-complete-reviewer-review-modal-trigger';
    }

    disclosureHistory(disclosureId) {
        return this._http.get(`${this._commonService.opaUrl}/opaDisclosureHistory/${disclosureId}`);
    }
    submitOPA(opaDisclosureId, opaDisclosureNumber) {
        return this._http.patch(`${this._commonService.opaUrl}/submit`, {opaDisclosureId, opaDisclosureNumber});
    }
    returnOPA(params) {
        return this._http.patch(this._commonService.opaUrl + '/return', params);
    }
    withdrawOPA(params) {
        return this._http.patch(this._commonService.opaUrl + '/withdraw', params);
    }

    completeOPAReview(OPADisclosureID, OPADisclosureNumber) {
        return this._http.patch(`${this._commonService.opaUrl}/complete/${OPADisclosureID}/${OPADisclosureNumber}`, {});
    }

    startReviewerReview(params: any) {
        return this._http.patch(`${this._commonService.opaUrl}/review/start`, params);
    }

    completeReviewerReview(params: any) {
        return this._http.patch(`${this._commonService.opaUrl}/review/complete`, params);
    }


    validateForm(configuration: any) {
        return this._http.post(this._commonService.formUrl + '/formbuilder/validateForm', configuration);
    }

}
