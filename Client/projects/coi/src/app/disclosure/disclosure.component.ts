import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, NavigationEnd, Router } from '@angular/router';
import { subscriptionHandler } from '../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { SfiService } from './sfi/sfi.service';
import { ApplicableQuestionnaire, COI, RO, getApplicableQuestionnaireData } from './coi-interface';
import { DataStoreService } from './services/data-store.service';
import { CoiService } from './services/coi.service';
import { Location } from '@angular/common';
import {
    deepCloneObject,
    hideModal,
    isEmptyObject,
    openModal,
} from '../../../../fibi/src/app/common/utilities/custom-utilities';
import { ElasticConfigService } from '../../../../fibi/src/app/common/services/elastic-config.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../fibi/src/app/app-constants';
import { CommonService } from '../common/services/common.service';
import {
    NO_DATA_FOUND_MESSAGE,
    REPORTER_HOME_URL,
    POST_CREATE_DISCLOSURE_ROUTE_URL,
    CREATE_DISCLOSURE_ROUTE_URL
} from '../app-constants';
import { NavigationService } from '../common/services/navigation.service';
import { getSponsorSearchDefaultValue, openCommonModal } from '../common/utilities/custom-utilities';
import { environment } from '../../environments/environment';
import { ModalType} from '../disclosure/coi-interface';
import { DefaultAssignAdminDetails, PersonProjectOrEntity, coiReviewComment } from '../shared-components/shared-interface';

@Component({
    selector: 'app-disclosure',
    templateUrl: './disclosure.component.html',
    styleUrls: ['./disclosure.component.scss'],
})


export class DisclosureComponent implements OnInit, OnDestroy {

    isCardExpanded = true;
    isCreateMode = false;
    isSaving = false;
    isCOIAdministrator = true;
    isAddAssignModalOpen = false;
    certificationText = 'I certify that the information provided for the Financial conflict of interest, including, responses to screening questions, list of my pertinent Significant Financial interests and possible relationship to my sponsored activity is an accurate and current statement of my reportable outside interests and activities.';
    $subscriptions: Subscription[] = [];
    coiData = new COI();
    currentStepNumber: 1 | 2 | 3 | 4 = 1;
    tempStepNumber: any;
    clickedOption: any;
    disclosureDetailsForSFI = { disclosureId: null, disclosureNumber: null };
    NO_DATA_FOUND_MESSAGE = NO_DATA_FOUND_MESSAGE;

    assignReviewerActionDetails: any = {};
    assignReviewerActionValidation = new Map();
    adminGroupsCompleterOptions: any = {};
    personElasticOptions: any = {};
    categoryClearFiled: String;
    assigneeClearField: String;
    isShowCountModal = false;
    selectedModuleCode: any;
    currentDisclosureId: any;
    inputType: string;
    disclosureSequenceStatusCode: any;
    personId: string;
    currentDisclosureNumber: number;
    disclosureType: string;
    coiList = [];
    prevURL = '';
    userDetails: any;
    userId: any;
    ispersondetailsmodal = false;
    disclosureId: number;
    disclosureNumber: number;
    disclosureStatusCode: string;
    fcoiTypeCode: any;
    deployMap = environment.deployUrl;
    isCOIReviewer = false;
    error = '';
    canShowReviewerTab = false;
    showConfirmation = false;
    relationshipError: any;
    questionnaireError: any;
    defaultAdminDetails = new DefaultAssignAdminDetails();
    personProjectDetails = new PersonProjectOrEntity();
    count: number;
    dependencies = ['coiDisclosure', 'numberOfSFI'];
    reviewStatus: string;
    filterType = 'ACTIVE';
    withdrawError = new Map();
    description: any;
    returnError = new Map();
    isShowMore = false;
    primaryBtnName = '';
    descriptionErrorMsg = '';
    textAreaLabelName = '';
    withdrawErrorMsg = 'Describe the reason for withdrawing the disclosure';
    returnErrorMsg = 'Describe the reason for returning the disclosure';
    helpTexts = [];
    withdrawHelpTexts = [
        `Withdraw any disclosure in 'Submitted' status.`,
        `Describe the reason for withdrawal in the field provided.`,
        `Click on 'Withdraw' button to recall your disclosure for any modification.`
    ];
    returnHelpTexts = [
        `Return any disclosure in 'Review in progress' status.`,
        `Describe the reason for returning  in the field provided.`,
        `Click on 'Return' button to return the disclosure for any modification.`
    ];
    isOpenRiskSlider = false;
    reviewList: any = [];

    constructor(public router: Router,
        public commonService: CommonService,
        private _route: ActivatedRoute,
        private _elasticConfigService: ElasticConfigService,
        public sfiService: SfiService,
        public coiService: CoiService,
        public location: Location,
        public dataStore: DataStoreService, public navigationService: NavigationService) {
        document.getElementById('COI_SCROLL').scrollTo(0, 0);
        this.isCreateMode = this.router.url.includes('create-disclosure');
        this.setStepFirstTime(this.router.url);
        this.$subscriptions.push(this.router.events.subscribe(event => {
            if (event instanceof NavigationEnd) {
                this.isCreateMode = event.url.includes('create-disclosure');
            }
        }));
    }

    ngOnInit() {
        this.personElasticOptions = this._elasticConfigService.getElasticForPerson();
        this.coiService.isCOIAdministrator = this.commonService.getAvailableRight(['MANAGE_FCOI_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE']);
        this.canShowReviewerTab = this.commonService.getAvailableRight(['MANAGE_DISCLOSURE_REVIEW', 'VIEW_DISCLOSURE_REVIEW']);
        this.getDataFromStore();
        this.getDisclosureTypeMessage();
        this.listenDataChangeFromStore();
        this.prevURL = this.navigationService.previousURL;
        this._route.queryParams.subscribe(params => {
            const MODULE_ID = params['disclosureId'];
            if (!MODULE_ID) {
                this.router.navigate([], {
                    queryParams: {
                        disclosureId: this.coiData.coiDisclosure.disclosureId
                    },
                    queryParamsHandling: 'merge',
                });
            }
        });
        this.updateTimeStampEvent();
        this.routerEventSubscription();
    }

    routerEventSubscription() {
        this.$subscriptions.push(this.router.events.subscribe(event => {
          if (event instanceof NavigationEnd) {
            this.setStepFirstTime(this.router.url);
          }
        }));
      }

    ngOnDestroy(): void {
        this.dataStore.dataChanged = false;
        this.coiService.isCOIAdministrator = false;
        subscriptionHandler(this.$subscriptions);
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this.dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    setStepFirstTime(currentUrl) {
        if (currentUrl.includes('create-disclosure/screening')) {
            this.currentStepNumber = 1;
        } else if (currentUrl.includes('create-disclosure/sfi')) {
            this.currentStepNumber = 2;
        } else if (currentUrl.includes('create-disclosure/relationship')) {
            this.currentStepNumber = 3;
        } else if (currentUrl.includes('create-disclosure/certification')) {
            this.currentStepNumber = 4;
        }
    }

    goToStep(stepPosition?: any) {
        if (this.dataStore.dataChanged) {
            this.tempStepNumber = stepPosition ? stepPosition : this.currentStepNumber + 1;
             openCommonModal('disclosure-unsaved-changes-modal');
        } else {
            if (!stepPosition && this.currentStepNumber === 4) {
                return;
            }
            this.currentStepNumber = stepPosition ? stepPosition : this.currentStepNumber + 1;
            this.navigateToStep();
        }
    }

    leavePageClicked() {
        this.dataStore.dataChanged = false;
        this.coiService.unSavedModules = '';
        this.currentStepNumber = this.tempStepNumber;
        this.navigateToStep();
    }

    stayOnPageClicked() {
        this.tempStepNumber = this.clickedOption === 'previous' ? this.currentStepNumber + 1 : this.currentStepNumber - 1;
    }

    goBackStep() {
        if (this.dataStore.dataChanged) {
            this.tempStepNumber = this.currentStepNumber - 1;
             openCommonModal('disclsoure-unsaved-changes-modal');
        } else {
            if (this.currentStepNumber === 1) {
                return;
            }
            this.currentStepNumber--;
            this.navigateToStep();
        }
    }

    isRouteComplete(possibleActiveRoutes: string[] = []) {
        return possibleActiveRoutes.some(paths => this.router.url.includes(paths));
    }

    getDisclosureTitleName(fcoiTypeCode: any) {
        switch (fcoiTypeCode) {
            case '1':
                return 'FCOI';
            case '2':
                return 'Proposal';
            case '3':
                return 'Award';
            case '4':
                return 'FCOI';
        }
    }

    navigateToStep() {
        let nextStepUrl = '';
        switch (this.currentStepNumber) {
            case 1:
                nextStepUrl = '/coi/create-disclosure/screening';
                this.router.navigate([nextStepUrl], { queryParamsHandling: 'preserve' });
                this.tempStepNumber = null;
                break;
            case 2:
                nextStepUrl = '/coi/create-disclosure/sfi';
                this.router.navigate([nextStepUrl], { queryParamsHandling: 'preserve' });
                this.tempStepNumber = null;
                break;
            case 3:
                nextStepUrl = '/coi/create-disclosure/relationship';
                this.router.navigate([nextStepUrl], { queryParamsHandling: 'preserve' });
                this.tempStepNumber = null;
                break;
            case 4:
                nextStepUrl = '/coi/create-disclosure/certification';
                this.router.navigate([nextStepUrl], { queryParamsHandling: 'preserve' });
                this.tempStepNumber = null;
                break;
            default:
                nextStepUrl = this.navigationService.navigationGuardUrl;
                this.router.navigateByUrl(this.navigationService.navigationGuardUrl);
                this.tempStepNumber = null;
                break;
        }
    }

    checkQuestionnaireCompletedBeforeCertify() {
        if (!this.isSaving) {
            this.isSaving = true;
            this.coiService.getApplicableQuestionnaire(this.getApplicationQuestionnaireRO())
                .subscribe((res: getApplicableQuestionnaireData) => {
                    this.certifyIfQuestionnaireCompleted(res);
                }, _err => {
                    this.isSaving = false;
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                });
        }
    }

    private certifyIfQuestionnaireCompleted(res: getApplicableQuestionnaireData) {
        if (res && res.applicableQuestionnaire && res.applicableQuestionnaire.length) {
            if (!this.isAllQuestionnaireCompleted(res.applicableQuestionnaire)) {
                this.error = 'Please complete the following mandatory Questionnaire(s) in the Screening Questionniare section.';
                this.coiService.submitResponseErrors.push(this.error);
            }
            this.validateRelationship();
        }
    }

    isAllQuestionnaireCompleted(questionnaires: ApplicableQuestionnaire[]) {
        return questionnaires.every(questionnaire => questionnaire.QUESTIONNAIRE_COMPLETED_FLAG === 'Y');
    }

    getApplicationQuestionnaireRO() {
        return {
            'moduleItemCode': 8,
            'moduleSubItemCode': 0,
            'moduleSubItemKey': 0,
            'moduleItemKey': this.coiData.coiDisclosure.disclosureId,
            'actionUserId': this.commonService.getCurrentUserDetail('personId'),
            'actionPersonName': this.commonService.getCurrentUserDetail('fullName'),
            'questionnaireMode': 'ACTIVE_ANSWERED_UNANSWERED'
        };
    }

    certifyDisclosure() {
        const REQUESTREPORTDATA = {
            coiDisclosure: {
                disclosureId: this.coiData.coiDisclosure.disclosureId,
                certificationText: this.coiData.coiDisclosure.certificationText ?
                    this.coiData.coiDisclosure.certificationText : this.certificationText,
                    conflictStatusCode: this.dataStore.disclosureStatus
            }
        };
        this.$subscriptions.push(this.coiService.certifyDisclosure(REQUESTREPORTDATA).subscribe((res: any) => {
            this.dataStore.dataChanged = false;
            this.dataStore.updateStore(['coiDisclosure'], { coiDisclosure: res });
            this.isSaving = false;
            this.router.navigate([POST_CREATE_DISCLOSURE_ROUTE_URL], { queryParamsHandling: 'preserve' });
            this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Disclosure Submitted Successfully.');
        }, err => {
            this.isSaving = false;
            if (err.status === 405) {
            hideModal('confirmModal');
            this.coiService.concurrentUpdateAction = 'Submit Disclosure';
          } else {
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Error In Certifying Disclosure.');
          }
        }));
    }
    validateRelationship() {
        this.$subscriptions.push(this.coiService.givecoiID(this.coiData.coiDisclosure.disclosureId).subscribe((res: any) => {
            res.map((error) => {
                this.coiService.submitResponseErrors.push( error.validationMessage) ;
            });
           this.getSfiDetails();
        }, err => {
            this.isSaving = false;
        }));
    }
    private getDataFromStore() {
        const coiData = this.dataStore.getData();
        if (isEmptyObject(coiData)) { return; }
        this.coiData = coiData;
        this.disclosureDetailsForSFI.disclosureId = this.coiData.coiDisclosure.disclosureId;
        this.disclosureDetailsForSFI.disclosureNumber = this.coiData.coiDisclosure.disclosureNumber;
        this.setAdminGroupOptions();
        this.setAssignAdminModalDetails();
    }

    changeDataStoreRisk(event) {
        this.coiData.coiDisclosure.riskCategoryCode = event.riskCategoryCode;
        this.coiData.coiDisclosure.coiRiskCategory = event.riskCategory;
        this.dataStore.setStoreData(this.coiData);
    }

    openAddAssignModal(): void {
        this.isAddAssignModalOpen = true;
        this.setAssignAdminModalDetails();
    }

    private setAssignAdminModalDetails(): void {
        this.defaultAdminDetails.adminGroupId = this.coiData.coiDisclosure.adminGroupId;
        this.defaultAdminDetails.adminGroupName = this.coiData.coiDisclosure.adminGroupName;
        this.defaultAdminDetails.adminPersonId = this.coiData.coiDisclosure.adminPersonId;
        this.defaultAdminDetails.adminPersonName = this.coiData.coiDisclosure.adminPersonName;
    }

    getDisclosureStatusBadgeTextColor(statusCode) {
        switch (statusCode) {
            case '1': return 'black';
            case '2':
            case '4':
            case '5':
                return 'white';
            case '3': case '6': return 'white';
            default: return 'white';
        }
    }

    completeDisclosureReview() {
        this.$subscriptions.push(this.coiService
            .completeDisclosureReview(this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.disclosureNumber)
            .subscribe((res: any) => {
                this.updateDisclosureReviewStatus(res.body.coiDisclosure);
                this.commonService.showToast(HTTP_SUCCESS_STATUS, `Review completed successfully.`);
            }, _err => {
                if (_err.status === 405) {
                    hideModal('completeReviewModalFromDashboard');
                    this.coiService.concurrentUpdateAction = 'Complete Review';
                } else {
                if (_err.error.text === 'REVIEW_STATUS_NOT_COMPLETE') {
                    document.getElementById('reviewPendingCompleteReviewErrorModalTrigger').click();
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, `Error in completing review.`);
                }
            }
            }));
    }

    updateDisclosureReviewStatus(res) {
        this.coiData.coiDisclosure = deepCloneObject(res);
        this.dataStore.updateStore(['coiDisclosure'], this.coiData);
    }

    triggerSave() {
        this.coiService.globalSave$.next();
    }

    private validateAssignReviewerAction() {
        this.assignReviewerActionValidation.clear();
        if (!this.assignReviewerActionDetails.assigneePersonId && !this.assignReviewerActionDetails.adminGroupId) {
            this.assignReviewerActionValidation.set('reviewer', 'Please select an admin group or assignee.');
        }
        return this.assignReviewerActionValidation.size === 0;
    }

    saveOrUpdateCoiReview() {
        if (this.validateAssignReviewerAction()) {
            this.assignReviewerActionDetails.disclosureId = this.coiData.coiDisclosure.disclosureId;
            this.$subscriptions.push(this.coiService
                .saveOrUpdateCoiReview({ coiReview: this.assignReviewerActionDetails }).subscribe((res: any) => {
                    this.assignReviewerActionDetails = {};
                    this.triggerAssignReviewerModal();
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, `Review added successfully.`);
                }, _err => {
                    this.commonService.showToast(HTTP_ERROR_STATUS, `Error in adding review.`);
                }));
        }
    }

    adminGroupSelect(event: any): void {
        this.assignReviewerActionDetails.adminGroupId = event ? event.adminGroupId : null;
        this.assignReviewerActionDetails.adminGroup = event ? event : null;
    }

    assigneeSelect(event: any): void {
        this.assignReviewerActionDetails.assigneePersonId = event ? event.prncpl_id : null;
        this.assignReviewerActionDetails.assigneePersonName = event ? event.full_name : null;
    }

    private setAdminGroupOptions(): void {
        this.adminGroupsCompleterOptions = {
            arrayList: this.getActiveAdminGroups(),
            contextField: 'adminGroupName',
            filterFields: 'adminGroupName',
            formatString: 'adminGroupName',
            defaultValue: ''
        };
    }

    private getActiveAdminGroups() {
        return this.coiData.adminGroup && this.coiData.adminGroup.filter(element => element.isActive === 'Y') || [];
    }

    triggerAssignReviewerModal() {
        this.assignReviewerActionDetails = {};
        this.assignReviewerActionValidation.clear();
        this.assigneeClearField = new String('true');
        this.categoryClearFiled = new String('true');
        const isReviewTab = this.router.url.includes('disclosure/review');
        document.getElementById(isReviewTab ?
            'add-review-modal-trigger' : 'assign-reviewer-modal-trigger').click();
    }

    openCountModal(moduleName, coiData, count = null) {
        if (count > 0) {
            switch (moduleName) {
                case 'sfi':
                    this.selectedModuleCode = 8;
                    break;
                case 'award':
                    this.selectedModuleCode = 1;
                    break;
                case 'proposal':
                    this.selectedModuleCode = 3;
                    break;
                default:
                    this.selectedModuleCode = 0;
            }
            this.fcoiTypeCode = coiData?.coiDisclosure?.coiDisclosureFcoiType?.fcoiTypeCode;
            this.isShowCountModal = true;
            this.currentDisclosureId = coiData?.coiDisclosure?.disclosureId;
            this.currentDisclosureNumber = coiData?.coiDisclosure?.disclosureNumber;
            this.disclosureType = moduleName;
            this.inputType = 'DISCLOSURE_TAB';
            this.disclosureSequenceStatusCode = coiData?.coiDisclosure?.disclosureStatusCode;
            this.personId = coiData?.coiDisclosure?.person?.personId;
        }
    }
    closeModal(event) {
        this.isShowCountModal = event;
    }

    openDetailModal(coiData: any): void {
        this.userDetails = coiData.coiDisclosure.person;
        this.ispersondetailsmodal = true;
    }
    closePersonDetailsModal(event) {
        this.ispersondetailsmodal = event;

    }

    goToHomeUrl() {
        // TODO admin/reviewer/pi based redirect once rights are implemented.
        const reRouteUrl = this.coiService.previousHomeUrl || REPORTER_HOME_URL;
        this.router.navigate([reRouteUrl]);
    }
    unitTitle() {
        return getSponsorSearchDefaultValue(this.coiData.coiDisclosure.person.unit);
    }

    closeAssignAdministratorModal(event) {
        if (event && (event.adminPersonId || event.adminGroupId)) {
            this.coiData.coiDisclosure.adminPersonId = event.adminPersonId;
            this.coiData.coiDisclosure.adminPersonName = event.adminPersonName;
            this.coiData.coiDisclosure.adminGroupId = event.adminGroupId;
            this.coiData.coiDisclosure.adminGroupName = event.adminGroupName;
            this.coiData.coiDisclosure.coiReviewStatusType.reviewStatusCode = event.reviewStatusCode;
            this.coiData.coiDisclosure.coiReviewStatusType.description = event.reviewStatus;
            this.coiData.coiDisclosure.reviewStatusCode = event.reviewStatusCode;
            this.coiData.coiDisclosure.updateTimestamp = new Date().getTime();
            this.getCoiReview();
            this.dataStore.updateStore(['coiDisclosure'], this.coiData);
        }
        this.isAddAssignModalOpen = false;
    }

    getCoiReview() {
        this.$subscriptions.push(this.coiService.getCoiReview(this.coiData.coiDisclosure.disclosureId).subscribe((data: any) => {
            if (data) {
                this.coiService.isReviewActionCompleted = this.coiService.isAllReviewsCompleted(data);
            }
            this.reviewList = data;
            this.coiService.isReviewActionCompleted = data.every(value => value.coiReviewStatus.reviewStatusCode === '4');
        }))
    }

    public updateCoiReview(modalType: ModalType) {
        const reviewerInfo = this.coiData.coiReviewerList.find(ele =>
            ele.assigneePersonId === this.commonService.currentUserDetails.personId);
        if (reviewerInfo) {
            this.coiService.$SelectedReviewerDetails.next(reviewerInfo);
            this.coiService.triggerStartOrCompleteCoiReview(modalType);
            this.coiService.isEnableReviewActionModal = true;
        }
    }

    errorCheck() {
        if (this.coiService.submitResponseErrors.length) {
            this.isSaving = false;
            openModal('ValidatedModal');
        } else {
            openModal('confirmModal');
        }
    }

    updateTimeStampEvent() {
        this.dataStore.updateTimestampEvent.subscribe((value) => {
            this.coiData.coiDisclosure.updateTimestamp = new Date().getTime();
            this.coiData = JSON.parse(JSON.stringify(this.coiData));
        });
    }

    getDisclosureTypeMessage(): string {
        const FCOITYPECODE = this.coiData?.coiDisclosure?.coiDisclosureFcoiType?.fcoiTypeCode;
        switch (FCOITYPECODE) {
            case '1': return 'FCOI';
            case '2': case '3': return 'Project';
            default: return ;
        }
    }

    getSfiDetails() {
        this.$subscriptions.push(this.sfiService.getSfiDetails(this.getRequestObject()).subscribe((data: any) => {
            if (data) {
                this.count = data.count;
                this.errorCheck();
            }
        }));
    }

    getRequestObject() {
      const REQ_OBJ = new RO();
      REQ_OBJ.currentPage = 0;
      REQ_OBJ.disclosureId = this.coiData.coiDisclosure.disclosureId;
      REQ_OBJ.filterType = this.filterType;
      REQ_OBJ.pageNumber = 0;
      REQ_OBJ.personId = this.coiData.coiDisclosure.person.personId;
      REQ_OBJ.reviewStatusCode = this.coiData.coiDisclosure.reviewStatusCode;
      REQ_OBJ.searchWord = '';
      return REQ_OBJ;
    }

    /**
     * 2 - Submitted
     * 3 - Review In Progress
     * 7 - Review Assigned
     * 8 - Assigned review completed
     * To be done - Admin group id check needs to be added.
     */
    checkForModifyRisk() {
        return ['2', '3', '7', '8'].includes(this.coiData.coiDisclosure.coiReviewStatusType.reviewStatusCode) &&
        (this.coiService.isCOIAdministrator || this.coiData.coiDisclosure.adminPersonId === this.commonService.getCurrentUserDetail('personId'));
    }

    withdrawDisclosure() {
        this.$subscriptions.push(this.coiService
            .withdrawDisclosure({
                disclosureId: this.coiData.coiDisclosure.disclosureId,
                description: this.description
            })
            .subscribe((res: any) => {
                this.coiData.coiDisclosure.coiReviewStatusType.reviewStatusCode = res.reviewStatusCode;
                this.coiData.coiDisclosure.coiReviewStatusType.description = res.reviewStatusDescription;
                this.coiData.coiDisclosure.reviewStatusCode = res.reviewStatusCode;
                this.router.navigate([CREATE_DISCLOSURE_ROUTE_URL],
                    { queryParams: { disclosureId: this.coiData.coiDisclosure.disclosureId } });
            }, _err => {
                if (_err.status === 405) {
                    hideModal('disclosure-confirmation-modal');
                    this.coiService.concurrentUpdateAction = 'Withdraw Disclosure';
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, `Error in withdrawing disclosure.`);
                }
            }));
    }

    returnDisclosure() {
        this.$subscriptions.push(this.coiService
            .returnDisclosure({
                disclosureId: this.coiData.coiDisclosure.disclosureId,
                description: this.description
            })
            .subscribe((res: any) => {
                this.commonService.showToast(HTTP_SUCCESS_STATUS, `Disclosure returned successfully.`);
                this.goToHomeUrl();
            }, _err => {
                if (_err.status === 405) {
                    hideModal('disclosure-confirmation-modal');
                    this.coiService.concurrentUpdateAction = 'Return Disclosure';
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, `Error in returning disclosure.`);
                }
            }));
    }

    openConfirmationModal(actionBtnName: string, helpTexts: string [] = [], descriptionErrorMsg: string = ''): void {
        this.helpTexts = helpTexts;
        this.primaryBtnName = actionBtnName;
        this.descriptionErrorMsg = descriptionErrorMsg;
        this.textAreaLabelName = actionBtnName === 'Withdraw' ? ' Withdrawal' : 'Return';
        this.setPersonProjectDetails();
        document.getElementById('disclosure-confirmation-modal-trigger-btn').click();
    }

    private setPersonProjectDetails(): void {
        this.personProjectDetails.personFullName = this.coiData?.coiDisclosure?.person?.fullName;
        this.personProjectDetails.projectDetails = this.coiData?.projectDetail;
        this.personProjectDetails.unitDetails = this.coiData?.coiDisclosure?.person.unit?.unitDetail;
    }

    performDisclosureAction(event): void {
        this.description = event;
        switch (this.primaryBtnName) {
            case 'Return':
                return this.returnDisclosure();
            case 'Withdraw':
                return this.withdrawDisclosure();
            default:
                return;
        }
    }

    openRiskSlider() {
        this.isOpenRiskSlider = true;
    }

    closeSlider(event) {
        this.isOpenRiskSlider = false;
    }

    getWarningClass(typeCode) {
        switch (typeCode) {
            case '1':
                return 'invalid';
            case '2':
                return 'medium-risk';
            case '3':
                return 'low-risk';
            default:
                return;
        }
    }

    getManageDisclosureRight(): boolean {
        const IS_FCOI_ADMINISTRATOR = this.commonService.getAvailableRight('MANAGE_FCOI_DISCLOSURE');
        const IS_PROJECT_ADMINISTRATOR = this.commonService.getAvailableRight('MANAGE_PROJECT_DISCLOSURE');
        switch (this.coiData?.coiDisclosure?.fcoiTypeCode) {
			case '1':
			case '4':
				return IS_FCOI_ADMINISTRATOR;
			case '2':
			case '3':
				return IS_PROJECT_ADMINISTRATOR;
		}
    }

    openReviewComment() {
        const COMMENT_META_DATA: coiReviewComment = {
            disclosureId: this.coiData.coiDisclosure.disclosureId,
            coiSectionsTypeCode: '3',
            documentOwnerPersonId: this.coiData.coiDisclosure.person.personId,
            coiSubSectionsId: null,
            headerName: '',
            componentSubRefId: null,
            coiSubSectionsTitle: null
        }
        this.commonService.$commentConfigurationDetails.next(COMMENT_META_DATA);
        this.coiService.isShowCommentNavBar = true;
    }

    closeReviewComment(event) {
        this.coiService.isShowCommentNavBar = event;
    }

    cancelConcurrency() {
        this.coiService.concurrentUpdateAction = '';
    }

}
