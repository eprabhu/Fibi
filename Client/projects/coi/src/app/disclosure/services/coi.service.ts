import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { BehaviorSubject, Subject } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { ApplicableQuestionnaire, CertifyDisclosureRO, ExpandCollapseSummaryBySection, RO, getApplicableQuestionnaireData } from '../coi-interface';
import { URL_FOR_DISCLOSURE_PROJECT } from '../../app-constants';

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
    isShowAttachmentInfo = true;
    stepTabName = '';
    isCertified = false;
    isReviewActionCompleted = false;
    $SelectedReviewerDetails = new BehaviorSubject<any>({});
    currentReviewForAction: any;
    isShowCommentNavBar = false;
    isCOIAdministrator = false;
    isStartReview = false;
    isCompleteReview = false;
    isDisclosureReviewer = false;
    isEnableReviewActionModal = false;
    actionButtonId = null;
    certificationResponseErrors = [];
    submitResponseErrors = [];
    concurrentUpdateAction = '';
    focusSFIId: any;
    focusModuleId: any;
    focusSFIRelationId: any;
    isRelationshipSaving = false;
    $isExpandSection = new Subject<{ section: string, isExpand: boolean }>();
    currentActiveQuestionnaire: any;
    isFromCertificationTab = false;
    isViewportVisibilityEnabled: any[] = [];
    isExpandSummaryBySection = new ExpandCollapseSummaryBySection();
    activeSectionId: 'COI801' | 'COI802' | 'COI803' | 'COI804' = 'COI801';

    constructor(
        private _http: HttpClient,
        private _commonService: CommonService,
    ) { }

    loadDisclosure(disclosureId: string) {
        return this._http.get(`${this._commonService.baseUrl}/fcoiDisclosure/fetch/${disclosureId}`);
    }

    certifyDisclosure(params: CertifyDisclosureRO) {
        return this._http.patch(`${this._commonService.baseUrl}/fcoiDisclosure/certifyDisclosure`, params);
    }

    evaluateDisclosureQuestionnaire(params: any) {
        return this._http.post(`${this._commonService.baseUrl}/evaluateDisclosureQuestionnaire`, params);
    }

    completeDisclosureReview(disclosureId: any, disclosureNumber: any) {
        return this._http.post(`${this._commonService.baseUrl}/completeDisclosureReview/${disclosureId}/${disclosureNumber}`, {});
    }

    triggerCommentModal(data: any) {
        this.triggerAddReviewComment$.next(data);
    }

    saveOrUpdateCoiReview(params: any) {
        return this._http.post(`${this._commonService.baseUrl}/saveOrUpdateCoiReview`, params);
    }

    getCoiProjectTypes() {
        return this._http.get(`${this._commonService.baseUrl}/getCoiProjectTypes`);
    }

    getApplicableQuestionnaire(requestObject: any) {
        return this._http.post(`${this._commonService.fibiUrl}/getApplicableQuestionnaire`, requestObject);
    }

    getCoiReview(disclosureId: number, dispositionStatusCode: string) {
        return this._http.post(`${this._commonService.baseUrl}/getCoiReview`, {disclosureId, dispositionStatusCode});
    }

    startCOIReview(params: any) {
        return this._http.post(`${this._commonService.baseUrl}/startCOIReview`, { coiReview: params });
    }

    completeReview(params: any) {
        return this._http.post(`${this._commonService.baseUrl}/completeCOIReview`, { coiReview: params });
    }

    triggerStartOrCompleteCoiReview(modalType: string) {
        this.actionButtonId = modalType === 'START' ? 'coi-start-reviewer-review-modal-trigger' : 'coi-complete-reviewer-review-modal-trigger';
    }

    evaluateValidation(disclosureId: number, disclosureNumber: number) {
        return this._http.get(`${this._commonService.baseUrl}/fcoiDisclosure/evaluateValidation/${disclosureId}/${disclosureNumber}`);
    }

    getSfiDetails(params: RO) {
        return this._http.post(`${this._commonService.baseUrl}/personEntity/fetch`, params);

    }
    withdrawDisclosure(params: any) {
        return this._http.post(`${this._commonService.baseUrl}/withdrawDisclosure`, params);
    }

    returnDisclosure(params: any) {
        return this._http.post(`${this._commonService.baseUrl}/returnDisclosure`, params);
    }

    disclosureHistory(disclosureId) {
        return this._http.get(`${this._commonService.baseUrl}/disclosureHistory/${disclosureId}`);
    }

    isAllReviewsCompleted(reviewerList): boolean {
        return reviewerList.every(value => value.reviewerStatusType && value.reviewerStatusType.reviewStatusCode === '2');
    }

    riskAlreadyModified(params: any) {
        return this._http.post(`${this._commonService.baseUrl}/fcoiDisclosure/riskStatus`, params);
    }

    getDisclosureProjectList(disclosureId: number) {
        return this._http.get(this._commonService.baseUrl + URL_FOR_DISCLOSURE_PROJECT.replace('{disclosureId}', disclosureId.toString()));
    }

    addTableBorder(projectList, headerElementId) {
        if (this.focusSFIRelationId) {
            const INDEX = projectList.findIndex(ele => ele.disclosureDetailsId == this.focusSFIRelationId);
            if (INDEX != -1) {
                if (INDEX == 0) {
                    const ELEMENT = document.getElementById(headerElementId);
                    ELEMENT.classList.add('border-bottom-0');
                } else {
                    const ELEMENT_ID = (projectList[INDEX - 1].disclosureDetailsId).toString();
                    const ELEMENT = document.getElementById(ELEMENT_ID);
                    ELEMENT.classList.add('border-bottom-0');
                }
            }
        }
    }

    setActiveSection(activeSectionId: 'COI801' | 'COI802' | 'COI803' | 'COI804', isExpand = true): void {
        this.activeSectionId = activeSectionId;
        this.isExpandSummaryBySection[activeSectionId] = isExpand;
    }

}

export function certifyIfQuestionnaireCompleted(res: getApplicableQuestionnaireData,) {
    let errorArray = [];
    if (res && res.applicableQuestionnaire && res.applicableQuestionnaire.length) {
        if (isAllMandatoryQuestionnaireNotCompleted(res.applicableQuestionnaire)) {
            let questionnaire_error = { validationMessage: '', validationType: "VE", mandatoryComplete: "false" };
            questionnaire_error.validationMessage = 'Please complete the mandatory Questionnaire(s) in the “Screening Questionnaire” section.';
            errorArray.push(questionnaire_error);
        }
        if (!isAllQuestionnaireCompleted(res.applicableQuestionnaire)) {
            let questionnaire_error = { validationMessage: '', validationType: "VW", };
            questionnaire_error.validationMessage = 'Please complete all the Questionnaire(s) in the “Screening Questionnaire” section.';
            errorArray.push(questionnaire_error);
        }
    }
    return errorArray;
}

function isAllMandatoryQuestionnaireNotCompleted(questionnaires: ApplicableQuestionnaire[]) {
    return questionnaires.some((element) => element.IS_MANDATORY === 'Y' && element.QUESTIONNAIRE_COMPLETED_FLAG !== 'Y');
}

function isAllQuestionnaireCompleted(questionnaires: ApplicableQuestionnaire[]) {
    return questionnaires.every(questionnaire => questionnaire.QUESTIONNAIRE_COMPLETED_FLAG === 'Y');
}

