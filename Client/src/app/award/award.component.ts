/** Last updated by Ramlekshmy on 28-01-2020 */
import { Component, OnInit, OnDestroy, ViewChild, ElementRef, OnChanges, HostListener } from '@angular/core';
import { CommonDataService } from './services/common-data.service';
import { CommonService } from './../common/services/common.service';
import { forkJoin, Subscription, SubscriptionLike as ISubscription } from 'rxjs';
import { OverviewService } from './overview/overview.service';
import { ActivatedRoute, Router } from '@angular/router';
import { AwardService } from './services/award.service';
import { slideInOut, fadeDown, slideHorizontalOverlay } from '../common/utilities/animations';
import { DEFAULT_DATE_FORMAT, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS, AWARD_LABEL, COMMON_APPROVE_LABEL, COMMON_RETURN_LABEL } from '../app-constants';
import { environment } from '../../environments/environment';
import { getSectionList } from './services/section-wise-utility';
import { NavigationService } from '../common/services/navigation.service';
import { subscriptionHandler } from '../common/utilities/subscription-handler';
import { BudgetDataService } from './budget-expenses/budget-data.service';
import { compareDatesWithoutTimeZone, getDateObjectFromTimeStamp, getDuration,
        parseDateWithoutTimestamp } from '../common/utilities/date-utilities';
import { WafAttachmentService } from '../common/services/waf-attachment.service';
import { AwardSection } from './award-comparison/comparison-constants';
import { Section } from './award-comparison/interfaces';
import { CommentsService } from './award-comparison/comment/comments.service';
import { fileDownloader, openInNewTab, pageScroll } from '../common/utilities/custom-utilities';
import { setFocusToElement } from '../common/utilities/custom-utilities';
import {concatUnitNumberAndUnitName} from '../common/utilities/custom-utilities'
declare var $: any;
@Component({
  selector: 'app-award',
  templateUrl: './award.component.html',
  styleUrls: ['./award.component.css'],
  animations: [slideInOut, fadeDown, slideHorizontalOverlay],
  providers: [WafAttachmentService]
})
export class AwardComponent implements OnInit, OnDestroy, OnChanges {

  @ViewChild('moreOptions', { static: false }) moreOptions: ElementRef;
  @ViewChild('closeModal', { static: true }) closeModal: ElementRef;

  sections: Array<Section> = AwardSection;
  lookupData: any = {};
  latestVersion: any;
  serviceRequestTypeCode = null;
  isClosuredescription: any;
  awardId: any;
  modalAproveHeading: any;
  deployUrl = environment.deployUrl;
  iseditAwarddescription = null;
  isUnsavedWarningModalBackToList = false;
  isCreateVariationRequest = false;
  isShowMoreOptions = false;
  isProjectClosure = false;
  isModifyAward = false;
  isSubmit = false;
  isProjectOutCome = false;
  isWithdrawAward = false;
  isCancelAward = false;
  serviceRequestTypes: any = [];
  uploadedFiles = [];
  uploadedFile = [];
  warningMsgObj: any = {};
  validationObject: any = {};
  childAwardObj: any = {};
  requestObject: any = {};
  result: any = {};
  currentUser = this._commonService.getCurrentUserDetail('userName');
  awardObject: any = {
    awardId: '',
    awardNumber: '',
  };
  serviceRequestObject: any = {
    awardNumber: '',
    subject: '',
    description: '',
    awardId: '',
    serviceRequestTypeCode: '',
    serviceRequestType: { description: '', typeCode: '' },
  };
  requestType;
  canUserSubmitAward: boolean;
  isEditAward = true;
  temporarySubject;
  temporaryDescription;
  isOverviewHighlighted = false;
  isAttachmentHighlighted = false;
  isDatesAndAmountHighlighted = false;
  isBudgetHighlighted = false;
  isCostShareHighlighted = false;
  isReportsHighlighted = false;
  isOutcomeHighlighted = false;
  isCustomDataHighlighted = false;
  isTermsHighlighted = false;
  isRolesHighlighted = false;
  isQuestionnaireHighlighted = false;
  isPaymentsHighlighted = false;
  isHierarchyHighlighted = false;
  isManpowerHighlighted = false;
  isEditVariation = false;
  isBudgetTab = false;
  sectionEditableList = [];
  map = new Map();
  isShowNotificationModal = false;
  $subscriptions: Subscription[] = [];
  private $budgeTabTrigger: ISubscription;
  isShowApproveDisapproveButton = false;
  isEmptyCommentArea = false;
  isVariationRequest = false;
  isActiveAward = false;
  @ViewChild('mainHeaders', { static: true }) mainHeaders: ElementRef;
  isGenerateAwardWBSNumber: any = false;
  tempActiveAwardId: any;
  tempRequestId: any;
  isValidationFLag = false;
  withdrawAwardObject: any = {};
  variationType = null;
  isButtonDisabled = false;
  debounceTimer: any;
  isHierarchyEdit = true;
  sectionDescription = null;
  sectionCode: any;
  isAwardCommentsModal = false;
  isShowVariationComments = false;
  isAddReviewComment: any = false;
  clearTextArea: any;
  isCreateAward = false;
  isSaving = false;
  setFocusToElement = setFocusToElement;
  fuderApprovalDateCopy: any;
  datePlaceHolder = DEFAULT_DATE_FORMAT;
  isVariationCardActive = true;
  canCreateVariationRequest = true;
  applicableQuestionnaire: any;
  validationMessage: any;
  copyOtherInformation = false;
  copyQuestionnaire = false;
  questionnaireList: any = [];
  isDeleteAward = false;
  deleteWarningMessages: any = [];
  canDeleteAward: boolean;
  helpText: any = {};
  isCreateVariationInfo = true;
  concatUnitNumberAndUnitName = concatUnitNumberAndUnitName;

  constructor(public _commonData: CommonDataService, private _overviewService: OverviewService, private route: ActivatedRoute,
    public _router: Router, public _commonService: CommonService, public _awardService: AwardService,
    public _navigationService: NavigationService, public _budgetDataService: BudgetDataService,
    private _wafAttachmentService: WafAttachmentService, private _commentsService: CommentsService) {
    document.addEventListener('mouseup', this.offClickHandler.bind(this));
    document.addEventListener('mouseup', this.offClickMainHeaderHandler.bind(this));
  }

  // The function is used for closing nav dropdown at mobile screen
  offClickMainHeaderHandler(event: any) {
    if (window.innerWidth < 992) {
      const ELEMENT = <HTMLInputElement>document.getElementById('navbarResponsive');
      if (!this.mainHeaders.nativeElement.contains(event.target)) {
        if (ELEMENT.classList.contains('show')) {
          document.getElementById('responsiveColapse').click();
        }
      }
    }
  }

  ngOnInit() {
    this.getAwardGeneralData();
    this.awardId = this.route.snapshot.queryParams['awardId'];
    if (this.result && Object.keys(this.result).length) {
      this._commonData.awardTitle.title = this.result.award.title;
      this.getPermissions();
      this.setHighlightedTabs();
      this.getSectionEditableList();
      this.getAwardEditableSections(this.result.sectionTypeCodes);
      this.setTemporaryDateValues();
    }
    this.isAwardActiveChange();
    this.triggerRightsPermissions();
    this.budgetTabTriggered();
    this.showApproveDisapprove();
    this.getSystemLevelPermission();
    this.checkForCurrentTab();
    this.isAddReviewComment = this._commonData.checkDepartmentLevelRightsInArray('REVIEW_COMMENTS_RIGHT');
    this.fetchHelpText();
  }

  ngOnChanges() {
    if (this.result) {
      this.setHighlightedTabs();
      this.getSectionEditableList();
    }
  }


  closeModalAndValidation() {
    this.clearVariationRequestObject();
    this.serviceRequestObject.description = null;
    this.iseditAwarddescription = null;
  }


  setTemporaryDateValues() {
    this._commonData.beginDate = this.result.award && this.result.award.beginDate;
    this._commonData.finalExpirationDate = this.result.award && this.result.award.finalExpirationDate;
    this._commonData.awardEffectiveDate = this.result.award && this.result.award.awardEffectiveDate;
  }

  async getSystemLevelPermission() {
    this.isGenerateAwardWBSNumber = await this._commonService.checkPermissionAllowed('CAN_GENERATE_AWARD_WBS');
  }
  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
    if (this.$budgeTabTrigger) { this.$budgeTabTrigger.unsubscribe(); }
    this._commonData.setAwardData(null);
    this._commonData.awardTitle.title = null;
    this.fuderApprovalDateCopy = null;
  }
  budgetTabTriggered() {
    this.$budgeTabTrigger = this._budgetDataService.isBudgetTabTrigger.subscribe((data: any) => {
      this.isBudgetTab = data ? true : false;
    });
  }
  getAwardEditableSections(sectionTypeCodes) {
    this.sectionEditableList = getSectionList(sectionTypeCodes);
  }
  isAwardActiveChange() {
    this.$subscriptions.push(this._awardService.isAwardActive.subscribe((data: any) => {
      this.setHighlightedTabs();
      this.getSectionEditableList();
      this.showApproveDisapprove();
    }));
  }
  triggerRightsPermissions() {
    this.$subscriptions.push(this._awardService.isAvailableRights.subscribe((data: any) => {
      this.getPermissions();
    }));
  }
  showApproveDisapprove() {
    this.isShowApproveDisapproveButton = this.result && this.result.canApproveRouting === '1' &&
      this.result.award.workflowAwardStatusCode === '2' &&
      !['11', '13'].includes(this.result.award.awardStatus.statusCode) ? true : false;
  }

  getSectionEditableList() {
    this.isHierarchyEdit = this._commonData.getSectionEditableFlag('116'); // hierarchy
  }
  /**
   * returns highlight permission w.r.t section code
   */
  setHighlightedTabs() {
    this.isOverviewHighlighted =
      (this._commonData.checkSectionHightlightPermission('101') ||
        this._commonData.checkSectionHightlightPermission('104') ||
        this._commonData.checkSectionHightlightPermission('105') ||
        this._commonData.checkSectionHightlightPermission('106') ||
        this._commonData.checkSectionHightlightPermission('112') ||
        this._commonData.checkSectionHightlightPermission('113') ||
        this._commonData.checkSectionHightlightPermission('117') ||
        this._commonData.checkSectionHightlightPermission('122') ||
        this._commonData.checkSectionHightlightPermission('123')) ? true : false;
    this.isBudgetHighlighted = (this._commonData.checkSectionHightlightPermission('102') ||
      this._commonData.checkSectionHightlightPermission('114') ? true : false); // Budget && Expenses
    this.isAttachmentHighlighted = this._commonData.checkSectionHightlightPermission('103'); // attachments
    this.isDatesAndAmountHighlighted = this._commonData.checkSectionHightlightPermission('108'); // Dates and amounts
    this.isCostShareHighlighted = this._commonData.checkSectionHightlightPermission('111'); // CostShare
    this.isReportsHighlighted = this._commonData.checkSectionHightlightPermission('109'); // Reports
    this.isTermsHighlighted = this._commonData.checkSectionHightlightPermission('110'); // Terms
    this.isRolesHighlighted = this._commonData.checkSectionHightlightPermission('107'); // Roles
    this.isOutcomeHighlighted = this._commonData.checkSectionHightlightPermission('115'); // Outcome
    this.isHierarchyHighlighted = this._commonData.checkSectionHightlightPermission('116'); // Hierarchy
    this.isCustomDataHighlighted = this._commonData.checkSectionHightlightPermission('120'); // CustomData
    this.isPaymentsHighlighted = this._commonData.checkSectionHightlightPermission('121'); // payments
    this.isQuestionnaireHighlighted = this._commonData.checkSectionHightlightPermission('124'); // questionnaire
    this.isManpowerHighlighted = this._commonData.checkSectionHightlightPermission('131') ||
      this._commonData.checkSectionHightlightPermission('132') || this._commonData.checkSectionHightlightPermission('133')
      || this._commonData.checkSectionHightlightPermission('134');
      // manpower 131 - for editing the manpower staff, 132 - for editing others section ,
      // 134 - for editing students section, 133 - for triggering staff
  }

  getAwardGeneralData() {
    this.$subscriptions.push(this._commonData.awardData.subscribe((data: any) => {
      if (data) {
        this.result = JSON.parse(JSON.stringify(data));
        this.result.award.funderApprovalDate = this.result.award.funderApprovalDate ?
         getDateObjectFromTimeStamp(this.result.award.funderApprovalDate) : null;
        this.fuderApprovalDateCopy = this.result.award.funderApprovalDate;
      }
    }));
  }

  /**
   * @param  {any} event
   * sets the section description and fetch its corresponding section code before adding the comment.
   */
  sectionChangeEvent(event: any) {
    this.sectionDescription = event ? event : null;
    if (this.sectionDescription === null) {
      this.getReviewSectionCode('General Award Information');
    }
    this.getReviewSectionCode(this.sectionDescription);
  }

  /**
   * @param  {string} sectionDescription
   * sets the reviewSectionCode for adding the comments to corresponding section and pass it to backend.
   */
  getReviewSectionCode(sectionDescription: string) {
    this.sections.find(element => {
      if (element.reviewSectionDescription === sectionDescription) {
        this.sectionCode = element.reviewSectionCode;
      }
    });
  }

  /**
   * @param  {any} sectionCode
   * For setting the dropdown value of each section from its section code passed.
   */
  getSectionDescriptionFromCode(sectionCode: any) {
    this.sectionDescription = this.sections.find(element => element.reviewSectionCode === sectionCode).reviewSectionDescription;
    this.setDynamicSectionDescription(this.sectionDescription);
  }

  /**
   * @param  {any} sectionName
   * dynamically sets the dropdown value with section when the user clicks 'Add Award Comments' option.
   */
  setDynamicSectionDescription(sectionName: any) {
    if (this._router.url.includes(sectionName)) {
      this.sectionDescription = sectionName;
    }
  }

  /**  Setting the section name(current tab name) to the dropdown value in 'Add Award Comment' modal if the user refresh the page. */
  checkForCurrentTab() {
    if (this._router.url.includes('other-information')) {
      this.getSectionDescriptionFromCode(120);
    } else if (this._router.url.includes('dates')) {
      this.getSectionDescriptionFromCode(108);
    } else if (this._router.url.includes('budget')) {
      this.getSectionDescriptionFromCode(102);
    } else if (this._router.url.includes('cost-share')) {
      this.getSectionDescriptionFromCode(111);
    } else if (this._router.url.includes('attachments')) {
      this.getSectionDescriptionFromCode(103);
    } else if (this._router.url.includes('questionnaire')) {
      this.getSectionDescriptionFromCode(124);
    } else if (this._router.url.includes('payments')) {
      this.getSectionDescriptionFromCode(121);
    } else if (this._router.url.includes('terms-approval')) {
      this.getSectionDescriptionFromCode(110);
    } else if (this._router.url.includes('project-outcome')) {
      this.getSectionDescriptionFromCode(115);
    } else {
      this.getSectionDescriptionFromCode(101);
    }
  }

  /** Clears the validation and text area values when user clicks the close button.*/
  clearCommentArea() {
    this.isAwardCommentsModal = false;
    this.isShowVariationComments = false;
  }

  /**
   * Hide more option dropdown on clicking
   */
  offClickHandler(event: any) {
    if (this.moreOptions) {
      if (!this.moreOptions.nativeElement.contains(event.target)) {
        this.isShowMoreOptions = false;
      }
    }
  }

  /**
   * Sets award status badge w.r.t status code
   */
  getBadgeByStatusCode(statusCode) {
    if (statusCode === '1') {
      return 'success';
    } else if (statusCode === '2' || statusCode === '4' || statusCode === '5') {
      return 'danger';
    } else if (statusCode === '3' || statusCode === '6') {
      return 'warning';
    } else {
      return 'info';
    }
  }

  getBadgeByBudgetStatusCode(statusCode) {
    if (statusCode === '10' || statusCode === '9') {
      return 'success';
    } else if (statusCode === '11' || statusCode === '8') {
      return 'danger';
    } else if (statusCode === '12' || statusCode === '5') {
      return 'warning';
    } else {
      return 'info';
    }
  }

  /**
  * Sets workflow status badge w.r.t workflow status code
  */
  getWorkFlowStatusCode(statusCode) {
    if (statusCode === '1' || statusCode === '4') {
      return 'info';
    } else if (statusCode === '2') {
      return 'warning';
    } else if (statusCode === '3') {
      return 'success';
    } else if (statusCode === '7') {
      return 'danger';
    } else {
      return 'info';
    }
  }

  getVersionStatusCode(statusCode) {
    if (statusCode === 'ACTIVE') {
      return 'success';
    } else if (statusCode === 'CANCELLED') {
      return 'danger';
    } else if (statusCode === 'PENDING') {
      return 'warning';
    } else {
      return 'info';
    }
  }
  /**
   * @param  {} statusCode
   * N => no feed
   * R => Success
   * E => Error
   * P => Pending for feed
   * F => feeded
   */
  getSapFeedStatusCode(statusCode) {
    if (statusCode === 'N') {
      return 'info';
    } else if (statusCode === 'R') {
      return 'success';
    } else if (statusCode === 'E') {
      return 'danger';
    } else {
      return 'warning';
    }
  }

  /**
   * Gets award loopup data w.r.t  awardId,personId and lead unit number
   */
  getAwardLookupData() {
    this.$subscriptions.push(this._overviewService.getLookupData({
      'awardId': null, 'personId': this._commonService.getCurrentUserDetail('personID'),
      'leadUnitNumber': this._commonService.getCurrentUserDetail('unitNumber')
    }).subscribe(data => {
      this.lookupData = data;
      if (this.lookupData.awardStatus && this.result.award) {
        const statusObject = this.lookupData.awardStatus.find(status =>
          status.statudCode === this.result.award.statusCode + '');
        this.result.award.statusName = statusObject.description;
      }
    }));
  }

  backToAwardListClick() {
    this._navigationService.navigationGuardUrl = '/fibi/dashboard/awardList';
    if (this._commonData.isAwardDataChange) {
      document.getElementById('awardTabChangebutton').click();
    } else {
      this._router.navigate(['/fibi/dashboard/awardList']);
    }
  }

  async getPermissions() {
    this.isCreateVariationRequest = this._commonData.checkDepartmentLevelRightsInArray('CREATE_VARIATION_REQUEST');
    this.isModifyAward = this._commonData.checkDepartmentLevelRightsInArray('MODIFY_AWARD');
    this.isProjectClosure = this._commonData.checkDepartmentLevelRightsInArray('INITIATE_PROJECT_CLOSURE');
    this.isSubmit = this._commonData.checkDepartmentLevelRightsInArray('SUBMIT_AWARD');
    this.isProjectOutCome = this._commonData.checkDepartmentLevelRightsInArray('MODIFY_AWARD_OUTCOME');
    this.isWithdrawAward = this._commonData.checkDepartmentLevelRightsInArray('WITHDRAW_AWARD');
    this.isCancelAward = this._commonData.checkDepartmentLevelRightsInArray('DISAPPROVE_VARIATION_REQUEST');
    this.isCreateAward = this._commonData.checkDepartmentLevelRightsInArray('CREATE_AWARD');
    this.isDeleteAward = await this._commonService.checkPermissionAllowed('DELETE_AWARD');
  }
  /**
   * Trigger validation in overview tab using Subject isMandatory and show submit popup w.r.t returned Subject isTrue
   */
  triggerValidation() {
    this._awardService.isMandatory.next(true);
    if (this._awardService.isTrue || this._awardService.isTrue === undefined) {
      this.evaluateValidation();
    }
  }
  /**
   * Check the duration in the case of project extension with previous active award
   * end date and show the difference in dates in validation modal
   */
  awardExtensionCheck() {
    if (this.result.serviceRequest && this.result.serviceRequest.typeCode === '1') {
      const warningObject: any = { validationType: 'VW', validationMessage: '' };
      const prevDate = this.result.previousExpirationDate;
      if (prevDate > this.result.award.finalExpirationDate) {
        warningObject.validationMessage = this.getTimeIntervalInDays(getDuration(this.result.award.finalExpirationDate, prevDate, false))
          + ' reduction has been requested. Please confirm this is correct before submitting.';
        this.setWarningList(warningObject);
      } else if (prevDate < this.result.award.finalExpirationDate) {
        warningObject.validationMessage = this.getTimeIntervalInDays(getDuration(prevDate, this.result.award.finalExpirationDate, false))
          + 'extension has been requested. Please confirm this is correct before submitting.';
        this.setWarningList(warningObject);
      } else {
        warningObject.validationMessage =
          'There is no change in the Project Duration. Please confirm this is correct before submitting.';
        this.setWarningList(warningObject);
      }
    }
  }


  getUnAnsweredList(list) {
    let modifiedQuestionList = '<ol>';
    list.forEach(element => { modifiedQuestionList += "<li>" + element + "</li>" });
    return modifiedQuestionList + '</ol>'
  }

  /**
  * seting date object in years moths and dates format
  */
  getTimeIntervalInDays(DATEOBJ) {
    let timeString = '';
    timeString = timeString.concat(DATEOBJ.durInYears !== 0 ? DATEOBJ.durInYears + ' year(s) ' : '');
    timeString = timeString.concat(DATEOBJ.durInMonths !== 0 ? DATEOBJ.durInMonths + ' month(s) ' : '');
    timeString = timeString.concat(DATEOBJ.durInDays !== 0 ? DATEOBJ.durInDays + ' day(s) ' : '');
    return timeString;
  }
  /**
   * inserting validation object in validation list
   * of business rule validation modal for a time being.
   */
  setWarningList(warningObject: any) {
    this.validationObject.validationMsg.push(warningObject);
    this.validationObject.warningList.push(warningObject);
  }

  evaluateValidation(validation = false) {
    this.isValidationFLag = validation;
    this.validationObject.errorList = [];
    this.validationObject.validationMsg = [];
    this.validationObject.warningList = [];
    const validationRequest: any = {
      moduleCode: 1,
      subModuleCode: 0,
      moduleItemKey: this.route.snapshot.queryParams['awardId'],
      subModuleItemKey: 0
    };
    if (!this.isSaving) {
      this.isSaving = true;
      this.$subscriptions.push((forkJoin(this._commonService.evaluateValidation(validationRequest),
      this.awardClosureQuestionnaireCheck())).subscribe((data: any) => {
        this.setValidationMessage(data[0]);
        this.awardExtensionCheck();
        this.triggerRuleOrSubmitModal();
        this.isSaving = false;
      }, err => { this.isSaving = false; }));
    }
  }

  setValidationMessage(message) {
    if (message.length) {
      message.forEach(validationMsg => this.validationObject.validationMsg.push(validationMsg));
    }
    if (this.validationObject.validationMsg.length > 0) {
        this.validationObject.validationMsg.forEach(element => {
        (element.validationType === 'VW') ? this.validationObject.warningList.push(element) : this.validationObject.errorList.push(element);
    });
    }
  }

  /**
   * typeCode = 7 -- Award Closure
   */
  awardClosureQuestionnaireCheck() {
    return new Promise((resolve, reject) => {
      if (this._commonData.getSectionEditableFlag('124')) {
      const list = [];
      const errorObject: any = { validationType: 'VE', validationMessage: '' };
      this.fetchSubModuleCode().forEach(subItemCode => this.setQuestionnaireRequestObject(subItemCode, list));
      this.$subscriptions.push(
        forkJoin(...list).subscribe(data => {
          this.questionnaireList = [];
          data.forEach((d: any) => this.combineQuestionnaireList(d.applicableQuestionnaire));
          const UnAnsweredQuestionnaireList = [];
          if (this.questionnaireList && this.questionnaireList.length) {
            this.questionnaireList.forEach(element => {
              if (element.IS_MANDATORY === 'Y' && element.QUESTIONNAIRE_COMPLETED_FLAG !== 'Y') {
                UnAnsweredQuestionnaireList.push(element.QUESTIONNAIRE_LABEL || element.QUESTIONNAIRE);
              }
            });
            if (UnAnsweredQuestionnaireList.length) {
              errorObject.validationMessage = 'Please complete the following mandatory questionnaire(s) in the "Questionnaire" section.'
                + this.getUnAnsweredList(UnAnsweredQuestionnaireList);
              this.validationObject.validationMsg.push(errorObject);
            }
          }
          resolve(true);
        }, error => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Evaluating closure questionnaire failed. Please try again.');
            reject();
          }, ));
        } else {
          resolve(true);
        }
      });
  }

  combineQuestionnaireList(newList) {
    this.questionnaireList = [...this.questionnaireList, ...newList];
  }


  setQuestionnaireRequestObject(subItemCode, list) {
    const requestObject: any = {};
    requestObject.moduleItemCode = 1;
    requestObject.moduleSubItemCode = subItemCode;
    requestObject.moduleItemKey = this.result.award.awardId,
    requestObject.moduleSubItemKey = 0;
    requestObject.actionPersonName = this._commonService.getCurrentUserDetail('userName');
    requestObject.actionUserId = this._commonService.getCurrentUserDetail('personID');
    list.push(this.getApplicableQuestionnaire(requestObject));
  }

  getApplicableQuestionnaire(requestObject) {
    requestObject = JSON.parse(JSON.stringify(requestObject));
    return this._awardService.getApplicableQuestionnaire(requestObject);
  }

    /**
   * typeCode = 7 -- Award Closure
   * StatusCode = 21 -- Submit Closure request
   */
     private fetchSubModuleCode() {
        switch (this.result.award.awardVariationTypeCode) {
          case '7': return [0, 5, 6];
          case '21': return [0, 5];
          default: return [0];
        }
    }


  triggerRuleOrSubmitModal() {
    if (this.isValidationFLag || this.validationObject.validationMsg.length) {
      document.getElementById('validation-award-btn').click();
    } else {
      document.getElementById('confirm-validation-btn').click();
    }
  }

  continueToSubmit() {
    this.validationObject = {};
    this.validationObject.validationMsg = [];
    $('#ValidateAwardModal').modal('hide');
    $('#ConfirmAwardModal').modal('show');
  }

  /**
   * @param  {} awardId
   * @param  {} awardNumber
   * Submit award after validation and set award main object and latest workflow version
   */
  submitAward(awardId, awardNumber) {
    if (!this.isSaving) {
      this.isSaving = true;
      this.awardObject.awardId = awardId;
      this.awardObject.awardNumber = awardNumber;
      this._commonService.isShowOverlay = true;
      this.$subscriptions.push(this._awardService.submitAward(this.awardObject).subscribe((data: any) => {
        this.setupAwardStoreData(data);
        this.result.pendingAwardsSummary = data.pendingAwardsSummary;
        this._awardService.$isQuestionnaireChange.next(true);
        if (this.result.workflow !== null) {
          this.latestVersion = this.result.workflow.workflowSequence;
        }
        // to call service for fetching budget details after submit - reloading changes(budget status & mode) in budget after submit
        if (this.isBudgetTab && this._budgetDataService.budgetId) {
          this._budgetDataService.loadBudgetByAwardId(awardId);
        }
        if (data.award.awardSequenceStatus === 'ACTIVE') {
          this._router.navigate(['fibi/award/overview'], { queryParams: { 'awardId': data.award.awardId } });
        }
        this._commonService.isShowOverlay = false;
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Award submitted successfully.');
        this.isSaving = false;
      }, err => {
          this._commonService.isShowOverlay = false;
          this._commonService.showToast(HTTP_ERROR_STATUS,  'Submit Award failed as another transaction is being processed in current Award. Please click Submit again');
          this.isSaving = false;
      }));
    }
  }

  approveAward() {
    this.requestObject = {};
    this.isEmptyCommentArea = false;
    this.modalAproveHeading = COMMON_APPROVE_LABEL;
    this.requestObject.actionType = 'A';
    const buttonId = this.returnConfirmModalButtonId();
    document.getElementById(buttonId).click();
  }

  disapproveAward() {
    const buttonId = this.returnConfirmModalButtonId();
    document.getElementById(buttonId).click();
    this.isEmptyCommentArea = false;
    this.requestObject = {};
    this.modalAproveHeading = COMMON_RETURN_LABEL;
    this.requestObject.actionType = 'R';
  }

  setAwardWorFlowRequestObject() {
    this.requestObject.personId = this._commonService.getCurrentUserDetail('personID');
    this.requestObject.workFlowPersonId = this._commonService.getCurrentUserDetail('personID');
    this.requestObject.updateUser = this._commonService.getCurrentUserDetail('userName');
    this.requestObject.awardId = this.result.award.awardId;
    this.requestObject.awardNumber = this.result.award.awardNumber;
    this.requestObject.approverStopNumber = null;
  }
  /**
   * @param  {} data
   * setup award common data the values that changed after the service call need to be updatedinto the store.
   * every service call wont have all the all the details as reponse so
   * we need to cherry pick the changes and update them to the store.
   */
  setupAwardStoreData(data) {
    this.result.award = data.award;
    this.result.workflow = data.workflow;
    this.result.workflowList = data.workflowList;
    this.result.canApproveRouting = data.canApproveRouting;
    this.result.submitUserFullName = data.submitUserFullName;
    this.result.updateTimeStamp = data.updateTimeStamp;
    this.result.serviceRequest = data.serviceRequest;
    this.result.awardPersons = data.award.awardPersons;
    this.updateCommonActions(data);
  }

  updateCommonActions(data) {
    this.updateAwardStoreData();
    this.getAwardEditableSections(data.sectionTypeCodes);
    this.setHighlightedTabs();
    this.getSectionEditableList();
    this.showApproveDisapprove();
  }

  updateAwardStoreData() {
    this.result = JSON.parse(JSON.stringify(this.result));
    this._commonData.setAwardData(this.result);
  }
  generateAwardBudgetReport() {
    this._budgetDataService.isBudgetPrintTrigger.next(true);
  }
  /** approves or disapproves award with respect to actiontype and
   * set award object, latest workflow  and show toasters w.r.t
   * response
   */
  maintainAwardWorkFlow() {
    this.setAwardWorFlowRequestObject();
    this.validateReturnRequest();
    if (!this.isEmptyCommentArea && !this.isSaving) {
      this.isSaving = true;
      if (!this._commonService.isWafEnabled) {
        this._commonService.isShowOverlay = true;
        this.$subscriptions.push(this._awardService.maintainAwardWorkFlow(this.requestObject, this.uploadedFile).subscribe((data: any) => {
          this.awardWorkFlowActions(data);
          this._commonService.isShowOverlay = false;
          this.isSaving = false;
        },
          err => {
            $('#approveDisapproveAwardModal').modal('hide');
            this.closeApproveDisapproveModal();
            this._commonService.isShowOverlay = false;
            this._commonService.showToast(HTTP_ERROR_STATUS,
                `Action failed as another transaction is being processed in current Award. Please click ${COMMON_APPROVE_LABEL.toLowerCase()}/${COMMON_RETURN_LABEL.toLowerCase()} again.`);
            this.isSaving = false;
          },
          () => {
            $('#approveDisapproveAwardModal ').modal('hide');
            this.showSuccessToast();
            this.closeApproveDisapproveModal();
            this.isSaving = false;
          }));
      } else {
        $('#approveDisapproveAwardModal').modal('hide');
        this.maintainAwardWorkFlowWaf();
        this.isSaving = false;
      }
    }
  }
  /** checks for award approved or disapproved with or without attachments.If there are attachments, calls the 'saveAttachment'
   *  function with parameters in waf service for splitting attachment,returns data.
   * Otherwise calls saveWafRequest function in wafAttachmentService*/
  async maintainAwardWorkFlowWaf() {
    this.requestObject.moduleCode = '1';
    if (this.uploadedFile.length > 0) {
      const data = await this._wafAttachmentService.saveAttachment(this.requestObject, null, this.uploadedFile,
        '/approveOrRejectAwardWorkflow', 'awardWorkflow', null);
      this.checkWorkFlowCompleted(data);
    } else {
      this.requestObject.isLastRequest = true;
      this._wafAttachmentService.saveWafRequest(this.requestObject, '/approveOrRejectAwardWorkflow').then(data => {
        this.checkWorkFlowCompleted(data);
      }).catch(error => {
        this.checkWorkFlowCompleted(error);
      });
    }
  }
  /**
   * @param  {} data
   * if data doesn't contains error, actions after completing the workflow are done
   * Otherwise shows error toast
   */
  checkWorkFlowCompleted(data) {
    if (data && !data.error) {
      this.awardWorkFlowActions(data);
      this.showSuccessToast();
    } else {
      this.showErrorToast();
    }
    this.closeApproveDisapproveModal();
  }
  /**
   * @param  {} data
   * actions to perform in common for both waf enabled and disabled services after getting response data
   */
  awardWorkFlowActions(data) {
    this.setupAwardStoreData(data);
    this.result.pendingAwardsSummary = data.pendingAwardsSummary;
    if (this.result.workflow !== null) {
      this.latestVersion = this.result.workflow.workflowSequence;
    }
    this._awardService.isRouteChangeTrigger.next(true);
    this._awardService.$isQuestionnaireChange.next(true);
    // to call service for fetching budget details after approve/disapprove
    // actions - reloading changes(budget status & mode) in budget after submit
    if (this.isBudgetTab && this._budgetDataService.budgetId) {
      this._budgetDataService.loadBudgetByAwardId(this.result.award.awardId);
    }
    if (data.award.awardSequenceStatus === 'ACTIVE') {
      this._router.navigate(['fibi/award/overview'], { queryParams: { 'awardId': data.award.awardId } });
    }
  }
  /**shows success toast based on approve or disapprove award*/
  showSuccessToast() {
    if (this.requestObject.actionType === 'A') {
      this._commonService.showToast(HTTP_SUCCESS_STATUS, `${AWARD_LABEL} ${COMMON_APPROVE_LABEL.toLowerCase()}d successfully.`);
    } else if (this.requestObject.actionType === 'R') {
      this._commonService.showToast(HTTP_SUCCESS_STATUS, `${AWARD_LABEL} ${COMMON_RETURN_LABEL.toLowerCase()}d successfully.`);
    }
  }
  /**shows error toast based on approve or disapprove award*/
  showErrorToast() {
    if (this.requestObject.actionType === 'A') {
      this._commonService.showToast(HTTP_ERROR_STATUS,
        `Waf blocked request for ${COMMON_APPROVE_LABEL.toLowerCase().slice(0, -1)}ing the ${AWARD_LABEL.toLowerCase()}`);
    } else if (this.requestObject.actionType === 'R') {
      this._commonService.showToast(HTTP_ERROR_STATUS, `Waf blocked request for ${COMMON_RETURN_LABEL.toLowerCase().slice(0, -1)}ing the ${AWARD_LABEL.toLowerCase()}`);
    }
  }
  /**
   * to make commetns mandatory for returning in the routelog
   */
  validateReturnRequest() {
    this.isEmptyCommentArea = this.requestObject.actionType === 'R' && !this.requestObject.approveComment;
  }

  /**
   * closes approve-disapprove modal
   * clear files and requestObject
   */
  closeApproveDisapproveModal() {
    $('#approveDisapproveModal').modal('hide');
    this.requestObject = {};
    this.uploadedFile = [];
  }
  /**
   * @param  {} files
   * Check file duplication ,if no duplication insert it into an array
   */
  fileDrop(files) {
    this.warningMsgObj.attachmentWarningMsg = null;
    let dupCount = 0;
    for (let index = 0; index < files.length; index++) {
      if (this.uploadedFile.find(dupFile => dupFile.name === files[index].name) != null) {
        dupCount = dupCount + 1;
        this.warningMsgObj.attachmentWarningMsg = '* ' + dupCount + ' File(s) already added';
      } else {
        this.uploadedFile.push(files[index]);
      }
    }
  }
  variationFilesDrop(files) {
    let dupFile = null;
    for (let index = 0; index < files.length; index++) {
      dupFile = this.uploadedFiles.find(file => file.name === files[index].name);
      if (dupFile != null) {
        this.warningMsgObj.attachment = '* ' + dupFile.name + ' already added';
      } else {
        this.uploadedFiles.push(files[index]);
      }
    }
  }

  /**
   * Get Variation lookup values
   */
  getVariationRequestTypes() {
    this.$subscriptions.push(this._awardService.getServiceRequestTypeBasedOnModule().subscribe((data: any) => {
      this.serviceRequestTypes = data;
    }));
  }

  deleteFromUploadedFileList(index) {
    this.uploadedFile.splice(index, 1);
    this.warningMsgObj.attachment = null;
  }
  /**
   * create new variation request w.r.t to variation type
   * and route to the new award view
   */
  saveVariationRequest() {
    this.setRequestObject(this.serviceRequestObject.subject);
    this.serviceRequestObject.isVariationRequest = true;
    this.setAttachmentTempObject();
    if (this.validateVariationRequest() && !this.isSaving) {
      this.isSaving = true;
      if (!this._commonService.isWafEnabled) {
        this.$subscriptions.push(this._awardService.saveVariationRequset(this.serviceRequestObject, this.uploadedFiles)
          .subscribe((data: any) => {
            if (!data.canCreateVariationRequest) {
              this.setPendingAwardObject(data);
            } else {
              $('#createVariationRequestModal').modal('hide');
              this.createVariationRequestActions(data);
            }
            this.isSaving = false;
          }, err => { this.isSaving = false; }));
      } else {
        this.resetWafObject();
        this._commonService.isManualLoaderOn = true;
        this._commonService.isShowOverlay = true;
        this.createVariationRequestWaf();
        this.isSaving = false;
      }
    }
  }
  navigateToPendingAward(type) {
    switch (type) {
      case 'V': $('#createVariationRequestModal').modal('hide'); break;
      case 'A': $('#editAwardSetModal').modal('hide'); break;
      case 'P': $('#createProjectClosureModal').modal('hide'); break;
      default: break;
    }
    this.clearVariationRequestObject();
  }

  navigateAward(awardId) {
    openInNewTab('award/overview?', ['awardId'], [awardId]);
  }
  /** checks for variation request created with or without attachments.If created with attachments, calls the 'saveAttachment'
   *  function with parameters in waf service for splitting attachment,returns data.
   * Otherwise calls saveWafRequest function in wafAttachmentService*/
  async createVariationRequestWaf() {
    this.serviceRequestObject.isLastUploadedFile = false;
    if (this.uploadedFiles.length > 0) {
      const data = await this._wafAttachmentService.saveAttachment(this.serviceRequestObject, null, this.uploadedFiles,
        '/createAwardVariationRequestForWaf', 'createVariation', this.serviceRequestObject.newAttachments);
      if (!data.canCreateVariationRequest) {
        this.setPendingAwardObject(data);
        this.removeLoader();
      } else {
        $('#createVariationRequestModal').modal('hide');
        this.checkVariationCreated(data);
      }
    } else {
      this.serviceRequestObject.serviceRequestAttachment = null;
      this.serviceRequestObject.isLastRequest = true;
      this.serviceRequestObject.isFirstTimeCreation = true;
      this._wafAttachmentService.saveWafRequest(this.serviceRequestObject, '/createAwardVariationRequestForWaf').then((data: any) => {
        if (!data.canCreateVariationRequest) {
          this.setPendingAwardObject(data);
          this.removeLoader();
        } else {
          $('#createVariationRequestModal').modal('hide');
          this.checkVariationCreated(data);
        }
      }).catch(error => {
        this.checkVariationCreated(error);
      });
    }
  }
  /**
  * @param  {} data
  * if data doesn't contains error,variation request is created.Otherwise shows error toast
  */
  checkVariationCreated(data) {
    this.removeLoader();
    if (data && !data.error) {
      this.createVariationRequestActions(data);
      this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Variation Request created successfully .');
    } else {
      this._commonService.showToast(HTTP_ERROR_STATUS, 'Waf blocked creating variation request');
      this.clearVariationRequestObject();
    }
  }

  removeLoader() {
    this._commonService.isShowOverlay = false;
    this._commonService.isShowLoader.next(false);
    this._commonService.isManualLoaderOn = false;
  }

  /**
  * @param  {} data
  * actions to perform in common for both waf enabled and disabled services after getting data
  */
  createVariationRequestActions(data) {
    this.result = data;
    this.serviceRequestObject.isVariationRequest = false;
    this.result.kpiTypes = data.kpiTypes;
    this.result.awardKpis = data.awardKpis;
    this.result.awardMileStones = data.awardMileStones;
    this.result.previousExpirationDate = data.previousExpirationDate;
    this.result.taskCount = data.taskCount;
    this.result.isGenerateWBSNumber = data.isGenerateWBSNumber;
    this.result.pendingAwardsSummary = data.pendingAwardsSummary;
    this.setupAwardStoreData(data);
    this.clearVariationRequestObject();
    this._router.navigate(['fibi/award/overview'], { queryParams: { 'awardId': this.result.award.awardId } });
  }

  validateVariationRequest() {
    this.map.clear();
    if (!this.serviceRequestObject.serviceRequestTypeCode || this.serviceRequestObject.serviceRequestTypeCode === 'null') {
      this.map.set('serviceRequestTypeCode', 'serviceRequestTypeCode');
    }
    if (!this.serviceRequestObject.subject) {
      this.map.set('subject', 'subject');
    }
    if (!this.serviceRequestObject.description) {
      this.map.set('serviceRequestDescription', 'serviceRequestDescription');
    }
    return this.map.size > 0 ? false : true;
  }

  setAttachmentTempObject() {
    const tempArrayForAdd = [];
    for (let uploadIndex = 0; uploadIndex < this.uploadedFiles.length; uploadIndex++) {
      const tempObjectForAdd: any = {};
      tempObjectForAdd.fileName = this.uploadedFiles[uploadIndex].name;
      tempObjectForAdd.contentType = this.uploadedFiles[uploadIndex].type;
      tempObjectForAdd.updateTimestamp = new Date().getTime();
      tempObjectForAdd.updateUser = this._commonService.getCurrentUserDetail('userName');
      tempArrayForAdd[uploadIndex] = tempObjectForAdd;
    }
    this.serviceRequestObject.newAttachments = tempArrayForAdd;
  }

  clearVariationRequestObject() {
    this.serviceRequestObject = {};
    this.serviceRequestObject.serviceRequestType = {};
    this.serviceRequestTypeCode = null;
    this.uploadedFiles = [];
    this.canCreateVariationRequest = true;
    this.map.clear();
  }

  /**
   * @param  {} selectedType
   * set selected request type object from request type look up
   */

  onRequestTypeChange(selectedType) {
    this.validationMessage = null;
    this.canCreateVariationRequest = true;
    this.serviceRequestObject.subject = selectedType === 'null' ? null :
      this.serviceRequestTypes.find(type =>
        type.description === selectedType).subject;
    this.serviceRequestObject.description = selectedType === 'null' ? null :
      this.serviceRequestTypes.find(type =>
        type.description === selectedType).instruction;
    this.serviceRequestObject.serviceRequestTypeCode = selectedType === 'null' ? null :
      this.serviceRequestTypes.find(type =>
        type.description === selectedType).typeCode;
    if (this.serviceRequestObject.serviceRequestTypeCode != null) {
      this.serviceRequestObject.serviceRequestType.typeCode = this.serviceRequestObject.serviceRequestTypeCode;
      this.serviceRequestObject.serviceRequestType.description = selectedType;
    }
  }

  /**
   * download award summary with respect to awardId
   */
  printAward() {
    this.$subscriptions.push(this._awardService.printAward(this.result.award.awardId).subscribe(data => {
      const tempData: any = data || {};
      fileDownloader(data, `${AWARD_LABEL} Summary-_ ${this.result.award.title}`, 'pdf');
    }));
  }

  /**
   * create new variation request for edit award
   * and route to the new award view
   */

  editAwardset(type) {
    this.iseditAwarddescription = null;
    if (this.serviceRequestObject.description && !this.isSaving) {
      this.isSaving = true;
      if (type === 'Edit') {
        this.serviceRequestObject.isAwardModification = true;
        this.serviceRequestObject.serviceRequestTypeCode = '6';
      } else {
        this.serviceRequestObject.isAwardOutcome = true;
        this.serviceRequestObject.serviceRequestTypeCode = '8';
      }
      this.isButtonDisabled = true;
      this.setRequestObject('Admin Correction');
      this.$subscriptions.push(this._awardService.saveVariationRequset(this.serviceRequestObject, null).subscribe((data: any) => {
        if (!data.canCreateVariationRequest) {
          this.setPendingAwardObject(data);
        } else {
          $('#editAwardSetModal').modal('hide');
          this.editAwardActions(data);
        }
        this.isSaving = false;
      },
        error => {
          this.isButtonDisabled = false;
          this.isSaving = false;
        }));
    } else {
      this.iseditAwarddescription = 'Please enter description';
      this.isSaving = false;
    }
    this.setHighlightedTabs();
  }

  /**
   * @param  {} data
   * actions to perform in common for both waf enabled and disabled services after getting response data of edit award
   */
  editAwardActions(data) {
    this.serviceRequestObject.isAwardModification = false;
    this.serviceRequestObject.isAwardOutcome = false;
    this.serviceRequestObject.description = null;
    this.serviceRequestObject.subject = null;
    this.iseditAwarddescription = null;
    this.isButtonDisabled = false;
    this.result = data;
    this.result.isGenerateWBSNumber = data.isGenerateWBSNumber;
    this.result.pendingAwardsSummary = data.pendingAwardsSummary;
    this.setupAwardStoreData(data);
    this._router.navigate(['fibi/award/overview'], { queryParams: { 'awardId': data.award.awardId } });
  }

  setRequestObject(subject) {
    this.serviceRequestObject.awardId = this.result.award.awardId;
    this.serviceRequestObject.awardNumber = this.result.award.awardNumber;
    this.serviceRequestObject.subject = subject;
  }

  /**
   * create new variation request w.r.t to variation type project closure
   * and route to the new award view
   */
  saveProjectClosure() {
    this.isClosuredescription = null;
    if (this.serviceRequestObject.description) {
      this.setRequestObject('Project Closure');
      this.serviceRequestObject.isProjectClosure = true;
      this.serviceRequestObject.serviceRequestTypeCode = '7';
      this.setAttachmentTempObject();
      if (!this._commonService.isWafEnabled) {
        this.$subscriptions.push(this._awardService.saveVariationRequset(this.serviceRequestObject, this.uploadedFiles)
          .subscribe((data: any) => {
            if (!data.canCreateVariationRequest) {
              this.setPendingAwardObject(data);
            } else {
              $('#createProjectClosureModal').modal('hide');
              this.projectClosureActions(data);
            }
          }));
      } else {
        this.createProjectClosureWaf();
      }
    } else {
      this.isClosuredescription = 'Please enter description';
    }
    this.setHighlightedTabs();
  }

  resetWafObject() {
    this.serviceRequestObject.fileContent = null;
    this.serviceRequestObject.fileName = null;
    this.serviceRequestObject.contentType = null;
    this.serviceRequestObject.fileTimestamp = null;
    this.serviceRequestObject.length = null;
    this.serviceRequestObject.remaining = null;
  }

  /** checks for project closure created with or without attachments.If created with attachments, calls the 'saveAttachment'
  * function with parameters in waf service for splitting attachment,returns data.
  * Otherwise calls saveWafRequest function in wafAttachmentService*/
  async createProjectClosureWaf() {
    if (this.uploadedFiles.length > 0) {
      const data = await this._wafAttachmentService.saveAttachment(this.serviceRequestObject, null, this.uploadedFiles,
        '/createAwardVariationRequestForWaf', 'createVariation', this.serviceRequestObject.newAttachments);
      if (!data.canCreateVariationRequest) {
        this.setPendingAwardObject(data);
        this.removeLoader();
      } else {
        $('#createProjectClosureModal').modal('hide');
        this.checkProjectClosureCreated(data);
      }
    } else {
      this.serviceRequestObject.isLastRequest = true;
      this.serviceRequestObject.isFirstTimeCreation = true;
      this._wafAttachmentService.saveWafRequest(this.serviceRequestObject, '/createAwardVariationRequestForWaf').then((data: any) => {
        if (!data.canCreateVariationRequest) {
          this.setPendingAwardObject(data);
          this.removeLoader();
        } else {
          $('#createProjectClosureModal').modal('hide');
          this.checkProjectClosureCreated(data);
        }
      }).catch(error => {
        this.checkProjectClosureCreated(error);
      });
    }
  }
  setPendingAwardObject(data) {
    this.canCreateVariationRequest = data.canCreateVariationRequest;
    this.validationMessage = data.message;
    this.isButtonDisabled = false;
    pageScroll('create-variation-request');
  }
  /**
   * @param  {} data
   * if data doesn't contains error,project closure is created.Otherwise shows error toast
   */
  checkProjectClosureCreated(data) {
    if (data && !data.error) {
      this.projectClosureActions(data);
    } else {
      $('#createProjectClosureModal').modal('hide');
      this._commonService.showToast(HTTP_ERROR_STATUS, `Waf blocked request for creating ${AWARD_LABEL.toLowerCase()} closure`);
    }
  }
  /**
   * @param  {} data
   * actions to perform common for both waf enabled and disabled services after creating project closure
   */
  projectClosureActions(data) {
    this.result = data;
    this.uploadedFiles = [];
    this.result.isGenerateWBSNumber = data.isGenerateWBSNumber;
    this.result.pendingAwardsSummary = data.pendingAwardsSummary;
    this.setupAwardStoreData(data);
    this.serviceRequestObject.isProjectClosure = false;
    this.clearVariationRequestObject();
    this._router.navigate(['fibi/award/overview'], { queryParams: { 'awardId': this.result.award.awardId } });
  }

  /**
   * @param  {} moduleItemKey
   * open selected award in new browser tab w.r.t awardId
   */
  viewParentAward(moduleItemKey) {
    this.tempActiveAwardId = moduleItemKey;
    if (this._commonData.isAwardDataChange) {
      document.getElementById('awardTabChangebutton').click();
    } else {
      this.gotoActiveAward(moduleItemKey);
    }
  }
  gotoActiveAward(moduleItemKey) {
    this.isEditVariation = false;
    this._commonData.isAwardDataChange = false;
    openInNewTab('award/overview?', ['awardId'], [moduleItemKey]);
  }

  /**
  * @param  {} requestId
  * open selected service request in new browser tab w.r.t requestId
  */
  viewServiceRequest(requestId) {
    this.tempRequestId = requestId;
    if (this._commonData.isAwardDataChange) {
      document.getElementById('awardTabChangebutton').click();
    } else {
      this.gotoVariationRequest(requestId);
    }
  }
  gotoVariationRequest(requestId) {
    this.isEditVariation = false;
    this._commonData.isAwardDataChange = false;
    openInNewTab('service-request?', ['serviceRequestId'], [requestId]);
  }
  /**
   * navigate to the selected path and set dataChange flag to false
   */
  navigateUsingRedirectRoute() {
    this._commonData.isAwardDataChange = false;
    this.redirectBasedOnQueryParam();
  }

  redirectBasedOnQueryParam() {
    this._router.navigateByUrl(this._navigationService.navigationGuardUrl);
  }
  withdrawAward(awardId, type) {
    this.isButtonDisabled = true;
    this.setWithdrawAwardObject(awardId, type);
    if (!this.isSaving) {
      this.isSaving = true;
      if (!this._commonService.isWafEnabled) {
        this.$subscriptions.push(this._awardService.withdrawAward(this.withdrawAwardObject, this.uploadedFiles).subscribe((data: any) => {
          this.setActionsAfterWithdraw(data);
          this.isSaving = false;
        },
          error => {
            this.setWithdrawError();
            this.isSaving = false;
          }));
      } else {
        this.withdrawAwardWaf();
        this.isSaving = false;
      }
    }
    this.setHighlightedTabs();
  }

  /** checks for withdraw award with or without attachments.If created with attachments, calls the 'saveAttachment'
  *  function with parameters in waf service for splitting attachment,returns data.
  * Otherwise calls saveWafRequest function in wafAttachmentService*/
  async withdrawAwardWaf() {
    this.withdrawAwardObject.moduleCode = 1;
    if (this.uploadedFiles.length > 0) {
      const data = await this._wafAttachmentService.saveAttachment(this.withdrawAwardObject, null, this.uploadedFiles,
        '/withdrawAwardForWaf', 'awardWithdraw', this.withdrawAwardObject.newAttachments);
      this.setActionsAfterWithdraw(data);
    } else {
      this._wafAttachmentService.saveWafRequest(this.withdrawAwardObject, '/withdrawAwardForWaf').then((data: any) => {
        this.setActionsAfterWithdraw(data);
      }).catch(error => {
        this.isButtonDisabled = false;
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Waf blocked ' +
          (this.withdrawAwardObject.cancelRequest ? 'cancel action' : 'withdraw action'));
      });
    }
  }

  setActionsAfterWithdraw(data) {
    const action = this.withdrawAwardObject.cancelRequest ? 'cancelled' : 'withdrawn';
    const type = this.result.award.awardDocumentTypeCode;
    this.isButtonDisabled = false;
    this.closeModal.nativeElement.click();
    this.withdrawAwardObject = {};
    this.uploadedFiles = [];
    this._commonService.showToast(HTTP_SUCCESS_STATUS, (type === '1' ? `${AWARD_LABEL} ` : type === '2' ? 'Admin Correction ' : type === '3'
    && this.result.award.serviceRequestType.typeCode !== 7 ? 'Variation Request ' : `${AWARD_LABEL} Closure `) + action + ' successfully');
    this.result = data;
    this._commonData.beginDate = data.award.beginDate;
    this._commonData.finalExpirationDate = data.award.finalExpirationDate;
    this._commonData.awardEffectiveDate = data.award.awardEffectiveDate;
    this.updateCommonActions(data);
    this._router.navigate(['fibi/award/overview'], { queryParams: { 'awardId': this.result.award.awardId } });
  }

  setWithdrawError() {
    const action = this.withdrawAwardObject.cancelRequest ? 'Cancel' : 'Withdraw';
    const type = this.result.award.awardDocumentTypeCode;
    this.isButtonDisabled = false;
    this.closeModal.nativeElement.click();
    this._commonService.showToast(HTTP_ERROR_STATUS, 'Unable to ' + action + (type === '1' ? ` ${AWARD_LABEL}` : type === '2' ?
      ' Admin Correction' : type === '3' &&
       this.result.award.serviceRequestType.typeCode !== 7 ? ' Variation Request ' : ` ${AWARD_LABEL} Closure `));
  }

  setWithdrawAwardObject(awardId, type) {
    const tempAttachment: any = [];
    this.withdrawAwardObject.awardId = awardId;
    for (let uploadIndex = 0; uploadIndex < this.uploadedFiles.length; uploadIndex++) {
      const tempObjectForAdd: any = {};
      tempObjectForAdd.typeCode = '100';
      tempObjectForAdd.narrativeStatusCode = 'C';
      tempObjectForAdd.fileName = this.uploadedFiles[uploadIndex].name;
      tempObjectForAdd.updateTimestamp = new Date().getTime();
      tempObjectForAdd.updateUser = this._commonService.getCurrentUserDetail('userName');
      tempAttachment[uploadIndex] = tempObjectForAdd;
    }
    this.withdrawAwardObject.awardAttachments = tempAttachment;
    this.withdrawAwardObject.commentTypeCode = '4';
    this.withdrawAwardObject.cancelRequest = (type === 'C') ? true : false;
    this.withdrawAwardObject.updateUser = this._commonService.getCurrentUserDetail('userName');
    this.withdrawAwardObject.actionType = 'C';
    this.withdrawAwardObject.workFlowPersonId = this.withdrawAwardObject.personId = this._commonService.getCurrentUserDetail('personID');
    this.withdrawAwardObject.awardNumber = this.result.award.awardNumber;
    this.withdrawAwardObject.leadUnitNumber = this.result.award.leadUnitNumber;
  }
  /**
  * @param  {} award
  * add  child award as the copy of selected award(general details,keyperson and project team)
  */
  addChildAward(award) {
    this.childAwardObj.parentAwardId = award.awardId;
    this.childAwardObj.parentAwardNumber = award.awardNumber;
    this.childAwardObj.loggedUserName = this._commonService.getCurrentUserDetail('userName');
    this.childAwardObj.copyOtherInformation = this.copyOtherInformation;
    this.childAwardObj.copyQuestionnaire = this.copyQuestionnaire;
    this.childAwardObj.acType = 'I';
    this.$subscriptions.push(this._awardService.maintainAward(this.childAwardObj).subscribe((data: any) => {
      if (data) {
        this._commonService.showToast(HTTP_SUCCESS_STATUS, `Child ${AWARD_LABEL} added successfully.`);
        this._awardService.isAwardTreeTrigger.next(true);
      }
    }));
    this.clearModalFlags();
  }

  clearModalFlags() {
    this.copyOtherInformation = false;
    this.copyQuestionnaire = false;
}

  editVariationRequest(serviceRequest) {
    const serviceRequestObject: any = {};
    serviceRequestObject.serviceRequestId = serviceRequest.serviceRequestId;
    serviceRequestObject.serviceRequestSubject = serviceRequest.subject;
    serviceRequestObject.serviceRequestDescription = serviceRequest.description;
    this.isEditVariation = false;
    this.$subscriptions.push(this._awardService.editVariationRequest(serviceRequestObject).subscribe((data: any) => {
      this._commonData.isAwardDataChange = false;
      if (data) {
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Variation Request successfully updated.');
      }
    }));
  }

  /**method to trigger modal if there's any unsaved changes before submit */
  triggerSubmitModal() {
    if (this._commonData.isAwardDataChange) {
      document.getElementById('submit-without-save-btn').click();
    } else {
      this.triggerValidation();
    }
  }

  /** method to trigger approve/return actions from confirmation modal of actions without saving  */
  onApproveOrReturnWithoutSave() {
    this._commonData.isAwardDataChange = false;
    if (this.modalAproveHeading === COMMON_APPROVE_LABEL) {
      this.approveAward();
    } else if (this.modalAproveHeading === COMMON_RETURN_LABEL) {
      this.disapproveAward();
    }
  }

  /* method to return button id which triggers corresponding modal (whether warning or approve/disapprove)*/
  returnConfirmModalButtonId() {
    const confirmModalId = this._commonData.isAwardDataChange ? 'approve-without-save-btn' : 'approve-btn';
    return confirmModalId;
  }

  /**
   * @param  {} event
   *  updating isShowNotificationModal flag after sending award notification
   */
  showAwardNotificationModal(event) {
    this.isShowNotificationModal = event;
  }
  deleteAttachment(index) {
    this.uploadedFiles.splice(index, 1);
    this.warningMsgObj.attachment = null;
  }

  saveAwardWorkflowStatusForSponsor() {
    if (!this.isSaving) {
      this.isSaving = true;
      this.$subscriptions.push(this._awardService.saveAwardWorkflowStatusForSponsor(
        {
          'awardId': this.result.award.awardId,
          'personId': this._commonService.getCurrentUserDetail('personID'),
          'updateUser': this.currentUser,
          'funderApprovalDate': parseDateWithoutTimestamp(this.result.award.funderApprovalDate),
        }
      ).subscribe((data: any) => {
        $('#sponsorAprovalModal').modal('hide');
        this.result.award = data.award;
        this.updateAwardStoreData();
        this.isSaving = false;
        this.showApproveDisapprove();
      }, err => { this.isSaving = false; }));
      this.map.clear();
    }
  }

  saveAwardWorkflowStatusForSponsorData(type: any): void {
    if (type === 'approvalInProgress'  && this.validateDates()) {
      this.saveAwardWorkflowStatusForSponsor();
    }
    if (type === 'holdForFunding') {
      this.saveAwardWorkflowStatusForSponsor(); }
  }

  cancelFunderApprovalDate(): void {
    this.result.award.funderApprovalDate = this.fuderApprovalDateCopy;
    this.map.clear();
  }

  validateDates() {
    this.map.clear();
    if (!this.result.award.funderApprovalDate) {
      this.map.set('funderApprovalDate', 'Please pick a Funder Approval Date.');
    }
    return this.map.size ? false : true;
  }

  copyAward() {
    const requestObject = {
      'awardId': this.result.award.awardId, 'isCopyAward': true,
      'createUser': this._commonService.getCurrentUserDetail('userName'), 'updateUser': this._commonService.getCurrentUserDetail('userName')
    };
    this.$subscriptions.push(this._awardService.copyAward(requestObject).subscribe((data: any) => {
      this.setupAwardStoreData(data);
      this._router.navigate(['fibi/award/overview'], { queryParams: { 'awardId': data.awardId } });
    }));
  }
  generateWbsNumber() {
    const requestObject = {
      'awardId': this.result.award.awardId
    };
    this.$subscriptions.push(this._awardService.generateWBSNumber(requestObject).subscribe((data: any) => {
      if (data) {
        this.setupAwardStoreData(data);
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'WBS Number successfully generated');
      }
    }, err => {
      this._commonService.showToast(HTTP_ERROR_STATUS, err.error);
    }));
  }
  editVariationRequestCardDetails() {
    this.isEditVariation = true;
    this.temporarySubject = this.result.serviceRequest.subject;
    this.temporaryDescription = this.result.serviceRequest.description;
  }
  removeVariationRequestCardDetails() {
    this.isEditVariation = false;
    this.result.serviceRequest.subject = this.temporarySubject;
    this.result.serviceRequest.description = this.temporaryDescription;
    this._commonData.isAwardDataChange = false;
  }
  generateAwardNotice() {
    this.$subscriptions.push(this._awardService.generateNotifyAwardReports(
      this.result.award.awardId, this.result.award.awardNumber, this.result.award.sequenceNumber
    ).subscribe(data => {
      this.result.isAwardNoticeGenerated = true;
    },
      err => {
        this._commonService.showToast(HTTP_ERROR_STATUS, err.error);
      },
      () => {
        this._commonService.showToast(HTTP_SUCCESS_STATUS, `${AWARD_LABEL} notice successfully generated`);
        this.setupAwardStoreData(this.result);
      }));
  }

  @HostListener('window:scroll', ['$event'])
  onWindowScroll() {
    const HEIGHT = document.getElementById('stickyAwardHeader').offsetHeight;
    const HEADER = document.getElementById('stickyAwardHeader');
    if (window.pageYOffset > HEIGHT && this._commonService.isIE) {
      HEADER.classList.add('tab-sticky');
    } else {
      HEADER.classList.remove('tab-sticky');
    }
  }
  /**
   * to show or hide task navigation bar
   */
  showNavBar() {
    this._commonData.isShowtaskNavBar = true;
  }

  cancelAwardComment() {
    this.isShowVariationComments = false;
    this.isAwardCommentsModal = false;
    $('#awardCommentsModal').modal('hide');
  }

  clearWarningMessages() {
    this.deleteWarningMessages = [];
  }

  deleteAward(): void {
    this.$subscriptions.push(this._awardService.deleteAward(this.result.award.awardId).subscribe((res: any) => {
      $('#deleteAwardModal').modal('hide');
      this._router.navigate(['fibi/dashboard/awardList']);
      this._commonService.showToast(HTTP_SUCCESS_STATUS, res.message);
      this.clearWarningMessages();
    }
      , err => {
        $('#deleteAwardModal').modal('hide');
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Delete Award failed as another transaction is being processed in current Award. Please try again.');
      }));
  }

  checkCanDeleteAward(): void {
    this.clearWarningMessages();
    this.$subscriptions.push(this._awardService.canDeleteAward(this.result.award.awardId).subscribe((res: any) => {
      this.canDeleteAward = res.status;
      if (this.canDeleteAward) {
        this.deleteWarningMessages.push('Are you sure you want to delete this Award?');
      } else {
        this.deleteWarningMessages = res.message;
      }
      $('#deleteAwardModal').modal('show');
    }
      , err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Delete Award failed as another transaction is being processed in current Award. Please try again.'); }));
  }

  fetchHelpText() {
    this.$subscriptions.push(this._awardService.fetchHelpText({
      'moduleCode': 1, 'sectionCodes': [197]
    }).subscribe((data: any) => {
      this.helpText = data;
    }));
  }

}

