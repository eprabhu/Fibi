import { Component, OnInit, Input, OnDestroy } from '@angular/core';
import { CommonService } from '../../../../common/services/common.service';
import { ProposalHomeService } from '../../proposal-home.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../app-constants';
import { compareDates, parseDateWithoutTimestamp, getDateObjectFromTimeStamp } from '../../../../common/utilities/date-utilities';
import { DEFAULT_DATE_FORMAT } from '../../../../app-constants';
import { scrollIntoView, setFocusToElement } from '../../../../common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../../../common/utilities/subscription-handler';
import { DataStoreService } from '../../../services/data-store.service';
import { AutoSaveService } from '../../../../common/services/auto-save.service';

declare var $: any;
@Component({
    selector: 'app-special-review',
    templateUrl: './special-review.component.html',
})

export class SpecialReviewDetailsComponent implements OnInit, OnDestroy {
    @Input() result: any = {};
    @Input() helpText: any = {};
    @Input() dataVisibilityObj: any = {};
    @Input() proposalDataBindObj: any = {};

    isShowSpecialReviewModal = true;
    isShowDeleteSpecialReviewModal = false;
    isSpecialReviewUpdate = false;
    selectedSpecialReviewType = null;
    specialReviewdateWarningMsg = null;
    specialReviewdateWarningMsgApproval = null;
    selectedSpecialReviewApprovalStatus = null;
    savedSpecialReviewObject: any = {};
    index: number;
    removeSpecialReviewId: string;
    map = new Map();
    specialReviewObject: any = {};
    requestObj: any = {};
    datePlaceHolder = DEFAULT_DATE_FORMAT;
    setFocusToElement = setFocusToElement;
    $subscriptions: Subscription[] = [];
    editIndex: any;
    isSaving = false;
    isShowLinkComplianceModal = false;
    isShowReviewResultCard = false;
    selectedReviewObject: any = null;
    viewProtocol: any;
    isViewProtocolDetails = false;
    specialReviewViewObject: any = {};
    integrationApprovalStatusDropdown: any = [];
    hasUnsavedChanges = false;

    constructor(public _commonService: CommonService,
        private _proposalHomeService: ProposalHomeService,
        private _dataStore: DataStoreService,
        private _autoSaveService: AutoSaveService) { }

    ngOnInit() {
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    /* shows special review modal */
    showAddSpecialReviewPopUp(e) {
        e.preventDefault();
        this.closeSpecialReview();
        this.isShowSpecialReviewModal = true;
    }

    /* assigns special review type */
    specialReviewTypeChange(type) {
        this.clearSearch();
        if (type === 'null') {
            this.specialReviewObject.specialReviewType = null;
            this.specialReviewObject.specialReviewTypeCode = null;
        } else {
            const specialReviewTypeObject = this.getSpecialReviewObject(type);
            if (specialReviewTypeObject != null) {
                this.specialReviewObject.specialReviewType = specialReviewTypeObject;
                this.specialReviewObject.specialReviewTypeCode = specialReviewTypeObject.specialReviewTypeCode;
            }
            if (type === '1') {
                this.integrationApprovalStatusDropdown = this.result.irbProtocolStatuses;
            }
            if (type === '2') {
                this.integrationApprovalStatusDropdown = this.result.acProtocolStatuses;
            }
        }
        this.clearSpecialReviewCard();
        this.setUnsavedChanges(true);
    }

    clearSpecialReviewCard() {
        this.isShowReviewResultCard = false;
        this.specialReviewObject.isProtocolIntegrated = false;
        this.selectedReviewObject = null;
    }

    getSpecialReviewObject(type: string): any {
        return this.result.reviewTypes.find(specialReviewType => specialReviewType.specialReviewTypeCode === type);
    }

    /* assigns special review approval status */
    specialReviewApprovalStatusChange(statusCode) {
        if (statusCode === 'null') {
            this.specialReviewObject.approvalType = null;
            this.specialReviewObject.approvalTypeCode = null;
        } else {
            this.specialReviewObject.comments = '';
            const specialReviewApprovalStatusObject = this.getApprovalStatusObject(statusCode);
            this.specialReviewObject.approvalType = specialReviewApprovalStatusObject;
            this.specialReviewObject.approvalTypeCode = specialReviewApprovalStatusObject.approvalTypeCode;
            if (statusCode === '4') {
                this.setCommentWhenExemptInIntegration(specialReviewApprovalStatusObject);
            }
        }
        this.setUnsavedChanges(true);
    }

    getApprovalStatusObject(statusCode: string): any {
        return this.result.specialReviewApprovalTypes.find(approvalType => approvalType.approvalTypeCode === statusCode);
    }

    /* date validation for special review application and expiration date */
    specialReviewDatevalidation() {
        this.specialReviewdateWarningMsg = null;
        if (this.specialReviewObject.applicationDate != null || this.specialReviewObject.expirationDate != null) {
            if (new Date(this.specialReviewObject.applicationDate) > new Date(this.specialReviewObject.expirationDate)) {
                if (this.specialReviewObject.expirationDate != null) {
                    this.specialReviewdateWarningMsg = '* Expiration Date should be on or after Application Date.';
                } else {
                    this.specialReviewdateWarningMsg = null;
                }
            } else {
                this.specialReviewdateWarningMsg = null;
            }
        }
    }

    /* adds special review */
    addSpecialReview() {
        if (this.result.proposal.proposalId == null) {
            this.dataVisibilityObj.isProposalSaved = false;
            $('#add-special-review-modal').modal('hide');
        } else {
            this.validateMandatoryFilled();
            if (this.map.size === 0 && this.specialReviewdateWarningMsg == null &&
                this.specialReviewdateWarningMsgApproval == null && !this.isSaving) {
                this.saveOrUpdateSpecialReview();
                this.resetCommentBox();
            }
        }
    }
    /**
     *resetCommentBox()
      Reset Special review Comment box height,
      In future changes shall be done in appAutoGrow directive to overcome this issue with comment height.
     */
    resetCommentBox() {
        document.getElementById('prop-special-revw-comnt').style.height = '50px';
    }

    setDateFormatWithoutTimeStamp() {
        this.specialReviewObject.applicationDate = parseDateWithoutTimestamp(this.specialReviewObject.applicationDate);
        this.specialReviewObject.approvalDate = parseDateWithoutTimestamp(this.specialReviewObject.approvalDate);
        this.specialReviewObject.expirationDate = parseDateWithoutTimestamp(this.specialReviewObject.expirationDate);
    }

    resetSpecialReviewType() {
        this.specialReviewObject.approvalType = null;
        this.specialReviewObject.approvalTypeCode = null;
        this.specialReviewObject.applicationDate = null;
        this.specialReviewObject.approvalDate = null;
        this.specialReviewObject.expirationDate = null;
    }

    saveOrUpdateSpecialReview() {
        this.specialReviewObject.proposalId = this.result.proposal.proposalId;
        this.setDateFormatWithoutTimeStamp();
        if (this.specialReviewObject.isProtocolIntegrated) {
            this.resetSpecialReviewType();
        }
        this.isSaving = true;
        this.$subscriptions.push(this._proposalHomeService.addSpecialReview({
            'proposalId': this.result.proposal.proposalId,
            'proposalSpecialReview': this.specialReviewObject,
            'updateUser': this._commonService.getCurrentUserDetail('userName')
        })
            .subscribe((data: any) => {
                this.result.proposalSpecialReviews = data;
                this._dataStore.updateStore(['proposalSpecialReviews'], this.result);
                this.isSaving = false;
            },
                err => {
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Adding special review failed. Please try again.');
                    this.isSaving = false;
                },
                () => {
                    this.dataVisibilityObj.isSpecialReviewWidgetOpen = true;
                    $('#add-special-review-modal').modal('hide');
                    if (this.isSpecialReviewUpdate) {
                        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Special Review updated successfully.');
                    } else {
                        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Special Review added successfully.');
                    }
                    this.closeSpecialReview();
                    this.isShowReviewResultCard = false;
                    this.isSaving = false;
                    this.specialReviewObject.isProtocolIntegrated = false;
                }));
    }

    /* closes special review modal */
    closeSpecialReview() {
        this.isShowSpecialReviewModal = true;
        this.isSpecialReviewUpdate = false;
        this.selectedSpecialReviewType = null;
        this.selectedSpecialReviewApprovalStatus = null;
        this.specialReviewdateWarningMsg = null;
        this.specialReviewObject = {};
        this.resetCommentBox();
        this.editIndex = null;
        this.resetSpecialReviewCard();
        this.setUnsavedChanges(false);
    }

    /* temporarary saves special review object before deletion */
    temprySpecialReviewObj(removeId, i) {
        this.removeSpecialReviewId = removeId;
        this.index = i;
        this.isShowDeleteSpecialReviewModal = true;
    }

    /* deletes special review details */
    deleteSpecialReview(e) {
        e.preventDefault();
        this.closeSpecialReview();
        this.isShowDeleteSpecialReviewModal = false;
        if (this.removeSpecialReviewId != null) {
            this.result.proposal.updateUser = this._commonService.getCurrentUserDetail('userName');
            this.requestObj.proposalId = this.result.proposal.proposalId;
            this.requestObj.proposalSpecialReviewId = this.removeSpecialReviewId;
            this.$subscriptions.push(this._proposalHomeService.deleteProposalSpecialReview(this.requestObj)
                .subscribe((data: any) => {
                    this.result.proposalSpecialReviews = data.proposalSpecialReviews;
                    this._dataStore.updateStore(['proposalSpecialReviews'], this.result);
                },
                    err => { this._commonService.showToast(HTTP_ERROR_STATUS, 'Removing special reviewfailed. Please try again.'); },
                    () => { this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Special Review removed successfully.'); }));
        }
    }

    /* assigns value to fields in special review modal for editing while clicking edit button */
    editSpecialReview(savedSpecialReviewObject, index) {
        this.editIndex = index;
        this.setUnsavedChanges(true);
        this.isShowReviewResultCard = false;
        this.specialReviewObject = JSON.parse(JSON.stringify(savedSpecialReviewObject));
        this.selectedSpecialReviewType = this.specialReviewObject.specialReviewType.specialReviewTypeCode;
        if (this.selectedSpecialReviewType === '1') {
            this.integrationApprovalStatusDropdown = this.result.irbProtocolStatuses;
        }
        if (this.selectedSpecialReviewType === '2') {
            this.integrationApprovalStatusDropdown = this.result.acProtocolStatuses;
        }
        this.selectedSpecialReviewApprovalStatus = this.specialReviewObject.approvalType.approvalTypeCode;
        this.specialReviewObject.applicationDate = getDateObjectFromTimeStamp(this.specialReviewObject.applicationDate);
        this.specialReviewObject.approvalDate = getDateObjectFromTimeStamp(this.specialReviewObject.approvalDate);
        this.specialReviewObject.expirationDate = getDateObjectFromTimeStamp(this.specialReviewObject.expirationDate);
        this.isSpecialReviewUpdate = true;
        this.isShowSpecialReviewModal = true;
        this.selectedReviewObject = null;
        this.selectedReviewObject = this.specialReviewObject.acProtocol ?
            this.specialReviewObject.acProtocol : this.specialReviewObject.irbProtocol;
        if (this.specialReviewObject.isProtocolIntegrated) {
            this.isShowReviewResultCard = true;
        }
        scrollIntoView('special-review-form');
    }

    showSpecialReviewComment(savedSpecialReviewObject) {
        this.savedSpecialReviewObject.comment = savedSpecialReviewObject.comments;
        this.savedSpecialReviewObject.title = savedSpecialReviewObject.specialReviewType.description;
    }

    updateSpecialReview() {
        this.validateMandatoryFilled();
        if (this.map.size === 0 && this.specialReviewdateWarningMsg == null) {
            this.saveOrUpdateSpecialReview();
            this.resetCommentBox();
        }
    }

    validateMandatoryFilled() {
        this.map.clear();
        if (!this.selectedSpecialReviewType || this.selectedSpecialReviewType === 'null') {
            this.map.set('specialReviewType', 'Type');
        }
        if (!this.selectedSpecialReviewApprovalStatus || this.selectedSpecialReviewApprovalStatus === 'null') {
            this.map.set('approvalStatus', 'Approval Status');
        }
    }

    specialReviewApprovalDateValidation() {
        this.specialReviewdateWarningMsgApproval = null;
        if (this.specialReviewObject.applicationDate && this.specialReviewObject.approvalDate) {
            this.specialReviewdateWarningMsgApproval =
                (compareDates(this.specialReviewObject.applicationDate, this.specialReviewObject.approvalDate)) === 1 ?
                    '* Approval date must be on or after Application Date.' : null;
        }
    }

    inputRestriction(event: any) {
        const pattern = (/^[0-9a-zA-Z\-]+$/);
        if (!pattern.test(String.fromCharCode(event.charCode))) {
            event.preventDefault();
        }
    }

    addNewReview() {
        this.isShowLinkComplianceModal = true;
        this.specialReviewObject.approvalTypeCode =
            this.specialReviewObject.approvalTypeCode ? this.specialReviewObject.approvalTypeCode : null;
    }

    specialReviewAdvSearchClick(event) {
        this.isShowLinkComplianceModal = false;
        if (event) {
            this.isShowReviewResultCard = true;
            this.specialReviewObject.isProtocolIntegrated = true;
            this.selectedReviewObject = event;
            this.specialReviewObject.protocolNumber = this.selectedReviewObject.protocolNumber;
            this.selectedSpecialReviewApprovalStatus = this.selectedReviewObject.protocolStatusCode;
            this.specialReviewObject.applicationDate = getDateObjectFromTimeStamp(this.selectedReviewObject.initialSubmissionDate);
            this.specialReviewObject.approvalDate = getDateObjectFromTimeStamp(this.selectedReviewObject.approvalDate);
            this.specialReviewObject.expirationDate = getDateObjectFromTimeStamp(this.selectedReviewObject.expirationDate);
            if (this.selectedSpecialReviewType === '1') {
                this.specialReviewObject.irbProtocol = event;
            }
            if (this.selectedSpecialReviewType === '2') {
                this.specialReviewObject.acProtocol = event;
            }
            this.integratedApprovalTypeChange();
            this.setUnsavedChanges(true);
        }
    }

    viewProtocolDetails(specialReview): void {
        this.specialReviewViewObject = specialReview;
    }

    resetSpecialReviewCard() {
        this.isShowReviewResultCard = false;
        this.specialReviewObject.isProtocolIntegrated = false;
        this.selectedReviewObject = {};
        this.specialReviewObject.approvalTypeCode = null;
        this.selectedSpecialReviewApprovalStatus = null;
        this.specialReviewObject.irbProtocol = this.specialReviewObject.acProtocol = null;
        this.specialReviewObject.approvalType = null;
    }

    clearSearch(): void {
        this.selectedSpecialReviewApprovalStatus = null;
        this.specialReviewObject.protocolNumber = null;
        this.specialReviewObject.comments = null;
        this.specialReviewObject.applicationDate = null;
        this.specialReviewObject.approvalDate = null;
        this.specialReviewObject.expirationDate = null;
        this.specialReviewObject.approvalTypeCode = null;
        this.integrationApprovalStatusDropdown = [];
    }

    closeViewModal(event) {
        this.isViewProtocolDetails = event;
        this.specialReviewViewObject = {};
    }

    integratedApprovalTypeChange(): void {
        this.specialReviewObject.approvalTypeCode = this.selectedSpecialReviewApprovalStatus;
        const STATUS_CHANGE =
            this.integrationApprovalStatusDropdown.find(S => S.protocolStatusCode === this.specialReviewObject.approvalTypeCode);
        if (this.specialReviewObject.approvalTypeCode === '4') {
            this.setCommentWhenExemptInIntegration(STATUS_CHANGE);
        }
    }

    setCommentWhenExemptInIntegration(specialReviewApprovalStatusObject) {
        this.specialReviewObject.comments = specialReviewApprovalStatusObject.description;
    }

    setUnsavedChanges(flag: boolean) {
        if (this.hasUnsavedChanges !== flag) {
            this._autoSaveService.setUnsavedChanges('Special Review', 'proposal-special-review', flag);
        }
        this.hasUnsavedChanges = flag;
    }

}

