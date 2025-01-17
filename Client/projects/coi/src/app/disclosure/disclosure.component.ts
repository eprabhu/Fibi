import { Component, HostListener, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, NavigationEnd, Router } from '@angular/router';
import { subscriptionHandler } from '../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { SfiService } from './sfi/sfi.service';
import { COI, CertifyDisclosureRO, RO, getApplicableQuestionnaireData } from './coi-interface';
import { DataStoreService } from './services/data-store.service';
import { CoiService, certifyIfQuestionnaireCompleted } from './services/coi.service';
import { Location } from '@angular/common';
import {
    deepCloneObject,
    hideModal,
    isEmptyObject,
    openModal,
} from '../../../../fibi/src/app/common/utilities/custom-utilities';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../fibi/src/app/app-constants';
import { CommonService } from '../common/services/common.service';
import {
    NO_DATA_FOUND_MESSAGE,
    REPORTER_HOME_URL,
    POST_CREATE_DISCLOSURE_ROUTE_URL,
    CREATE_DISCLOSURE_ROUTE_URL, COI_REVIEW_STATUS_TYPE, COI_CONFLICT_STATUS_TYPE,
    EXTERNAL_QUESTIONAIRE_MODULE_SUB_ITEM_CODE,
    COMMON_ERROR_TOAST_MSG,
    DISCLOSURE_TYPE
} from '../app-constants';
import { NavigationService } from '../common/services/navigation.service';
import { openCommonModal } from '../common/utilities/custom-utilities';
import { environment } from '../../environments/environment';
import { ModalType} from './coi-interface';
import { COICountModal, DefaultAssignAdminDetails, DisclosureProjectData, PersonProjectOrEntity, coiReviewComment } from '../shared-components/shared-interface';
import { ElasticConfigService } from '../common/services/elastic-config.service';

@Component({
    selector: 'app-disclosure',
    templateUrl: './disclosure.component.html',
    styleUrls: ['./disclosure.component.scss'],
})

export class DisclosureComponent implements OnInit, OnDestroy {

    isCardExpanded = false;
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
    coiList = [];
    prevURL = '';
    userId: any;
    disclosureId: number;
    disclosureNumber: number;
    disclosureStatusCode: string;
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
    withdrawErrorMsg = 'Please provide the reason for withdrawing the disclosure.';
    returnErrorMsg = 'Please provide the reason for returning the disclosure.';
    helpTexts = '';
    confirmationHelpTexts = '';
    isHomePageClicked = false;
    showSlider = false;
    selectedType: '';
    withdrawHelpTexts = 'Please provide the reason for withdrawal.';
    returnHelpTexts = 'Please provide the reason for return.';
    completeReviewHelpText = 'You are about to complete the disclosure\'s final review.'
    returnModalHelpText = '';
    withdrawModalHelpText = '';
    isOpenRiskSlider = false;
    reviewList: any = [];
    DISCLOSURE_TYPE = DISCLOSURE_TYPE;
    COI_CONFLICT_STATUS_TYPE = COI_CONFLICT_STATUS_TYPE;
    COI_REVIEW_STATUS_TYPE = COI_REVIEW_STATUS_TYPE;
    EXTERNAL_QUESTIONAIRE_MODULE_SUB_ITEM_CODE = EXTERNAL_QUESTIONAIRE_MODULE_SUB_ITEM_CODE;
    // CoiConflictStatusType = CoiConflictStatusType;
    // CoiReviewStatusType = CoiReviewStatusType;
    commentsRight: any = {};
    isUserCollapse = false;
    submitHelpTexts = '';
    fcoiTypeCode = '';
    coiCountModal = new COICountModal();

    constructor(public router: Router,
        public commonService: CommonService,
        private _route: ActivatedRoute,
        private _elasticConfigService: ElasticConfigService,
        public sfiService: SfiService,
        public coiService: CoiService,
        public location: Location,
        public dataStore: DataStoreService, public navigationService: NavigationService) {
        window.scrollTo(0, 0);
        this.isCreateMode = this.router.url.includes('create-disclosure');
        this.setStepFirstTime(this.router.url);
        this.$subscriptions.push(this.router.events.subscribe(event => {
            if (event instanceof NavigationEnd) {
                this.isCreateMode = event.url.includes('create-disclosure');
            }
        }));
    }

    ngOnInit() {
        this.listenScreenSize();
        this.personElasticOptions = this._elasticConfigService.getElasticForPerson();
        this.coiService.isCOIAdministrator = this.commonService.getAvailableRight(['MANAGE_FCOI_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE']);
        this.canShowReviewerTab = this.commonService.getAvailableRight(['MANAGE_DISCLOSURE_REVIEW', 'VIEW_DISCLOSURE_REVIEW']);
        // this.commentsRight.canViewPrivateComments = this.commonService.getAvailableRight(['VIEW_FCOI_PRIVATE_COMMENTS']);
        // this.commentsRight.canMaintainPrivateComments = this.commonService.getAvailableRight(['MAINTAIN_FCOI_PRIVATE_COMMENTS']);
        this.getDataFromStore();
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
        this.routerEventSubscription();
        this.setPersonProjectDetails();
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
        if (this.coiService.isRelationshipSaving) { return; }
        this.isHomePageClicked = false;
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
        this.setFocus('step_'+stepPosition);
    }

    leavePageClicked() {
        this.dataStore.dataChanged = false;
        this.coiService.unSavedModules = '';
        this.currentStepNumber = this.tempStepNumber;
        !this.isHomePageClicked ? this.navigateToStep() : this.router.navigate(['/coi/user-dashboard']);

    }

    stayOnPageClicked() {
        this.tempStepNumber = this.clickedOption === 'previous' ? this.currentStepNumber + 1 : this.currentStepNumber - 1;
    }

    goBackStep() {
        this.isHomePageClicked = false;
        if (this.dataStore.dataChanged) {
            this.tempStepNumber = this.currentStepNumber - 1;
            openCommonModal('disclosure-unsaved-changes-modal');
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

    getDisclosureTitleName(fcoiTypeCode: any): string {
        const { coiDisclosureFcoiType, coiProjectType} = this.coiData?.coiDisclosure;
        return fcoiTypeCode == DISCLOSURE_TYPE.PROJECT ? coiProjectType?.description : coiDisclosureFcoiType?.description;
    }

    navigateToStep() {
        let nextStepUrl = '';
        this.isHomePageClicked = false;
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
        this.coiService.submitResponseErrors = [];
        if (!this.isSaving) {
            this.isSaving = true;
            this.coiService.getApplicableQuestionnaire(this.getApplicationQuestionnaireRO())
                .subscribe((res: getApplicableQuestionnaireData) => {
                    this.checkQuestionnaireCompleted(res);
                }, _err => {
                    this.isSaving = false;
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                });
        }
    }

    checkQuestionnaireCompleted(res) {
        let errorArray = certifyIfQuestionnaireCompleted(res);
        if(errorArray && errorArray.length) {
            errorArray.forEach(ele => this.coiService.submitResponseErrors.push(ele));
        }
        this.validateRelationship();
    }

    getApplicationQuestionnaireRO() {
        return {
            'moduleItemCode': 8,
            'moduleSubItemCode': 0,
            'moduleSubItemKey': 0,
            'moduleItemKey': this.coiData.coiDisclosure.disclosureId,
            'actionUserId': this.commonService.getCurrentUserDetail('personID'),
            'actionPersonName': this.commonService.getCurrentUserDetail('fullName'),
            'questionnaireMode': 'ACTIVE_ANSWERED_UNANSWERED'
        };
    }

    certifyDisclosure() {
        const REQUESTREPORTDATA: CertifyDisclosureRO = {
            disclosureId: this.coiData.coiDisclosure.disclosureId,
            certificationText: this.coiData.coiDisclosure.certificationText ? this.coiData.coiDisclosure.certificationText : this.certificationText,
            conflictStatusCode: this.dataStore.disclosureStatus
        };
        this.$subscriptions.push(this.coiService.certifyDisclosure(REQUESTREPORTDATA).subscribe((res: any) => {
            this.dataStore.dataChanged = false;
            this.dataStore.updateStore(['coiDisclosure'], { coiDisclosure: res });
            this.isSaving = false;
            this.router.navigate([POST_CREATE_DISCLOSURE_ROUTE_URL], { queryParamsHandling: 'preserve' });
            this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Disclosure Submitted Successfully.');
            this.coiService.submitResponseErrors = [];
        }, err => {
            this.isSaving = false;
            if (err.status === 405) {
            hideModal('confirmModal');
            this.coiService.submitResponseErrors = [];
            this.coiService.concurrentUpdateAction = 'Submit Disclosure';
          } else {
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Error In Certifying Disclosure.');
          }
        }));
    }
    validateRelationship() {
        this.$subscriptions.push(
            this.coiService.evaluateValidation(this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.disclosureNumber)
            .subscribe((res: any) => {
                res.map((error) => {
                    this.coiService.submitResponseErrors.push( error) ;
                });
                this.getSfiDetails();
            }, err => {
                this.isSaving = false;
                if (err.status === 405) {
                    this.coiService.concurrentUpdateAction = 'Submit Disclosure';
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
                }
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
        if(this.coiData.coiDisclosure) {
            this.fcoiTypeCode = this.coiData.coiDisclosure.coiDisclosureFcoiType.fcoiTypeCode;
            const DISCLOSURE_TEXT = this.fcoiTypeCode != DISCLOSURE_TYPE.REVISION ? ' disclosure.' : '.';
            this.submitHelpTexts = 'You are about to submit the ' + this.getDisclosureTitleName(this.fcoiTypeCode).toLowerCase() + DISCLOSURE_TEXT;
            this.returnModalHelpText = 'You are about to return the ' + this.getDisclosureTitleName(this.fcoiTypeCode).toLowerCase() + DISCLOSURE_TEXT;
            this.withdrawModalHelpText = 'You are about to withdraw the ' + this.getDisclosureTitleName(this.fcoiTypeCode).toLowerCase() + DISCLOSURE_TEXT;
        }
    }

    changeDataStoreRisk(event) {
        this.coiData.coiDisclosure.riskCategoryCode = event.riskCategoryCode;
        this.coiData.coiDisclosure.coiRiskCategory = event.riskCategory;
        this.dataStore.updateStore(['coiDisclosure'], { coiDisclosure: this.coiData.coiDisclosure });
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

    completeDisclosureReview() {
        this.$subscriptions.push(this.coiService
            .completeDisclosureReview(this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.disclosureNumber)
            .subscribe((res: any) => {
                this.updateDisclosureReviewStatus(res);
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

    private updateDisclosureReviewStatus(res: any): void {
        this.coiData.coiDisclosure = deepCloneObject(res);
        this.dataStore.updateStore(['coiDisclosure'],  { coiDisclosure: this.coiData.coiDisclosure });
        this.router.navigate([POST_CREATE_DISCLOSURE_ROUTE_URL], { queryParamsHandling: 'preserve' });
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

    // currently this fuction is not using. It is for future
    openCountModal(moduleCode: number, count = 0): void {
        if (count > 0) {
            const { person, coiDisclosureFcoiType, coiProjectType, disclosureId } = this.coiData?.coiDisclosure;
            this.coiCountModal = {
                moduleCode: moduleCode,
                personUnit: person?.unit,
                personId: person?.personId,
                disclosureId: disclosureId,
                inputType: 'DISCLOSURE_TAB',
                personFullName: person?.fullName,
                fcoiTypeCode: this.fcoiTypeCode,
                disclosureType: this.fcoiTypeCode === DISCLOSURE_TYPE.PROJECT ? coiProjectType?.description : coiDisclosureFcoiType?.description,
                isOpenCountModal: true
            };
        }
    }

    openPersonDetailsModal(coiData: any): void {
        this.commonService.openPersonDetailsModal(coiData.coiDisclosure.person.personId)
    }

    goToHomeUrl() {
        // TODO admin/reviewer/pi based redirect once rights are implemented.
        const reRouteUrl = this.coiService.previousHomeUrl || REPORTER_HOME_URL;
        this.router.navigate([reRouteUrl]);
    }

    goBackInEditMode() {
        this.isHomePageClicked = true;
        if (this.dataStore.dataChanged) {
           openCommonModal('disclosure-unsaved-changes-modal');
        } else {
            this.router.navigate(['/coi/user-dashboard']);
        }
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
            this.coiData.coiDisclosure.updateTimestamp = event.updateTimestamp;
            this.getCoiReview();
            this.dataStore.updateStore(['coiDisclosure'], this.coiData);
        }
        this.isAddAssignModalOpen = false;
    }

    getCoiReview() {
        this.$subscriptions.push(this.coiService.getCoiReview(this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.dispositionStatusCode).subscribe((data: any) => {
            if (data) {
                this.coiService.isReviewActionCompleted = this.coiService.isAllReviewsCompleted(data);
                this.reviewList = data;
            }
        }))
    }

    public updateCoiReview(modalType: ModalType) {
        const reviewerInfo = this.coiData.coiReviewerList.find(ele =>
            ele.assigneePersonId === this.commonService.currentUserDetails.personID && ele.reviewStatusTypeCode != '2');
        if (reviewerInfo) {
            this.coiService.$SelectedReviewerDetails.next(reviewerInfo);
            this.coiService.triggerStartOrCompleteCoiReview(modalType);
            this.coiService.isEnableReviewActionModal = true;
        }
    }

    errorCheck() {
        if (this.coiService.submitResponseErrors.length && this.coiService.submitResponseErrors.find(data => data.validationType == "VE")) {
            this.isSaving = false;
            openModal('ValidatedModal');
        } else {
            this.isSaving = false;
            openModal('confirmModal');
        }
    }

    getSfiDetails() {
        this.$subscriptions.push(this.sfiService.getSfiDetails(this.getRequestObject()).subscribe((data: any) => {
            if (data) {
                this.count = data.count;
                this.errorCheck();
            }
        }, (_error: any) => {
            this.commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
        }));
    }

    getRequestObject(): RO {
      const REQ_OBJ = new RO();
      REQ_OBJ.currentPage = 0;
      REQ_OBJ.disclosureId = this.coiData.coiDisclosure.disclosureId;
      REQ_OBJ.filterType = this.filterType;
      REQ_OBJ.pageNumber = 0;
      REQ_OBJ.personId = this.coiData.coiDisclosure.person.personId;
      REQ_OBJ.reviewStatusCode = this.coiData.coiDisclosure.reviewStatusCode;
      REQ_OBJ.searchWord = '';
      REQ_OBJ.dispositionStatusCode = this.coiData.coiDisclosure.dispositionStatusCode;
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
        (this.coiService.isCOIAdministrator || this.coiData.coiDisclosure.adminPersonId === this.commonService.getCurrentUserDetail('personID'));
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

    openConfirmationModal(actionBtnName: string, helpTexts: string = '', descriptionErrorMsg: string = '', modalHelpText: string = ''): void {
        this.primaryBtnName = actionBtnName;
        this.descriptionErrorMsg = descriptionErrorMsg;
        this.confirmationHelpTexts = '';
        this.helpTexts = '';
        setTimeout(() => {
            this.confirmationHelpTexts = modalHelpText;
            this.helpTexts = helpTexts;
            document.getElementById('disclosure-confirmation-modal-trigger-btn').click();
        });
        this.textAreaLabelName = actionBtnName === 'Withdraw' ? ' Withdrawal' : 'Return';
        this.setPersonProjectDetails();
    }

    private setPersonProjectDetails(): void {
        this.personProjectDetails.personFullName = this.coiData?.coiDisclosure?.person?.fullName;
        this.personProjectDetails.projectDetails = this.coiData?.projectDetail;
        this.personProjectDetails.homeUnit = this.coiData?.coiDisclosure?.person?.unit?.unitNumber;
        this.personProjectDetails.homeUnitName = this.coiData?.coiDisclosure?.person?.unit?.unitName;
        this.personProjectDetails.personEmail = this.coiData?.coiDisclosure?.person?.emailAddress;
        this.personProjectDetails.personPrimaryTitle = this.coiData?.coiDisclosure?.person?.primaryTitle;
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
        this.$subscriptions.push(this.coiService.riskAlreadyModified({
            'riskCategoryCode': this.coiData.coiDisclosure.riskCategoryCode,
            'disclosureId': this.coiData.coiDisclosure.disclosureId
        }).subscribe((data: any) => {
            this.isOpenRiskSlider = true;
        }, err => {
            if (err.status === 405) {
                this.coiService.concurrentUpdateAction = 'Disclosure Risk Status';
            } else {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
            }
        }))
    }

    closeSlider(event) {
        this.isOpenRiskSlider = false;
    }

    getManageDisclosureRight(): boolean {
        const IS_FCOI_ADMINISTRATOR = this.commonService.getAvailableRight('MANAGE_FCOI_DISCLOSURE');
        const IS_PROJECT_ADMINISTRATOR = this.commonService.getAvailableRight('MANAGE_PROJECT_DISCLOSURE');
        switch (this.fcoiTypeCode) {
			case DISCLOSURE_TYPE.INITIAL:
			case DISCLOSURE_TYPE.REVISION:
				return IS_FCOI_ADMINISTRATOR;
			case DISCLOSURE_TYPE.PROJECT:
				return IS_PROJECT_ADMINISTRATOR;
		}
    }

    openReviewComment() {
        const COMMENT_META_DATA: coiReviewComment = {
            documentOwnerPersonId: this.coiData.coiDisclosure.person.personId,
            componentTypeCode: '3',
            subModuleItemKey: null,
            subModuleItemNumber : null
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

    openSlider(type, count) {
        if(count) {
            this.showSlider = true;
            this.selectedType = type;
        }
    }

    closeHeaderSlider(event: any): void {
        if (event?.concurrentUpdateAction) {
            this.coiService.concurrentUpdateAction = event.concurrentUpdateAction;
        } else {
            this.showSlider = false;
            this.selectedType = '';
        }
    }

    @HostListener('window:resize', ['$event'])
    listenScreenSize() {
        if(!this.isUserCollapse) {
            this.isCardExpanded = window.innerWidth > 1399;
        }
        this.commonService.$globalEventNotifier.next({ uniqueId: 'COI_DISCLOSURE_HEADER_RESIZE', content: { isCardExpanded: this.isCardExpanded, isResize: true }});
    }

    setFocus(id) {
        const focusedElement = document.getElementById(id);
        if(focusedElement) {
            setTimeout(() => {
                focusedElement.focus();
            },500)
        }
    }

    collapseHeader() {
        this.isCardExpanded = !this.isCardExpanded;
        this.isUserCollapse = !this.isUserCollapse;
        setTimeout (() => {
            this.commonService.$globalEventNotifier.next({ uniqueId: 'COI_DISCLOSURE_HEADER_RESIZE', content: { isCardExpanded: this.isCardExpanded, isResize: false }});
        });
    }

    openProjectHierarchySlider(): void {
        this.commonService.openProjectHierarchySlider(this.coiData?.projectDetail?.projectTypeCode, this.coiData?.projectDetail?.projectNumber);
    }

}
