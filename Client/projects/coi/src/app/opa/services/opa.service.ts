import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {CommonService} from '../../common/services/common.service';
import {of, Subject} from 'rxjs';

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
}
