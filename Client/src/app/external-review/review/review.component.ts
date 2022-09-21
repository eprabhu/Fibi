import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { Observable, Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { compareDates, getDateObjectFromTimeStamp, removeTimeZoneFromDateObject } from '../../common/utilities/date-utilities';
import { ExternalReviewService } from '../external-review.service';
import { QuestionnaireService } from '../questionnaire/questionnaire.service';
import { parseDateWithoutTimestamp } from '../../common/utilities/date-utilities';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { setFocusToElement } from '../../common/utilities/custom-utilities';

declare var $: any;
@Component({
    selector: 'app-review-ext',
    templateUrl: './review.component.html',
    styleUrls: ['./review.component.css']
})
export class ReviewComponent implements OnInit, OnDestroy {

    @Input() reviewTypes: any;
    @Input() reviewDetails: any;
    @Input() emitChildResponse: Observable<any>;
    @Output() sendForReview: EventEmitter<any> = new EventEmitter<any>();
    availableRights: string[] = this._commonService.externalReviewRights;

    isSaving = false;
    isViewMode = false;
    tabName = 'QUESTIONNAIRE';
    mandatoryList = new Map();
    $subscriptions: Subscription[] = [];
    tempReviewData: any;
    deadLineDateValidation: string;
    setFocusToElement = setFocusToElement;

    constructor(public _commonService: CommonService,
        public _questService: QuestionnaireService,
        public _reviewService: ExternalReviewService) { }

    ngOnInit() {
        this.tempReviewData = JSON.parse(JSON.stringify(this.reviewDetails));
        this.reviewDetails.deadlineDate = getDateObjectFromTimeStamp(this.reviewDetails.deadlineDate);
        this.setViewMode();
        this.deadlineDateValidation();
        this.updateUserDetailsFromParent();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    checkMandatoryFilled() {
        this.mandatoryList.clear();
        if (!this.reviewDetails.extReviewServiceTypeCode || this.reviewDetails.extReviewServiceTypeCode === 'null') {
            this.mandatoryList.set('type', '* Please choose a review type.');
        }
        if (!this.reviewDetails.deadlineDate) {
            this.mandatoryList.set('date', '* Please add a deadline date.');
        }
        if (!this.reviewDetails.description) {
            this.mandatoryList.set('description', '* Please add description.');
        }
        this.deadlineDateValidation();
        return this.mandatoryList.size !== 0 ? false : true;
    }

    updateReview() {
        if (this.checkMandatoryFilled() && !this.isSaving) {
            this.isSaving = true;
            this.reviewDetails.deadlineDate = parseDateWithoutTimestamp(this.reviewDetails.deadlineDate);
            this.$subscriptions.push(this._reviewService
                .updateExternalReview({
                    ...this.reviewDetails,
                    ...this._reviewService.moduleDetails,
                    'isTypeChange': this.tempReviewData.extReviewServiceType.isScoringNeeded
                }).subscribe((data: any) => {
                    this.reviewDetails = data;
                    this.isSaving = false;
                    this.tempReviewData = JSON.parse(JSON.stringify(this.reviewDetails));
                    this.reviewDetails.deadlineDate = getDateObjectFromTimeStamp(this.reviewDetails.deadlineDate);
                    this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Review updated successfully');
                    this.setViewMode();
                    this.deadlineDateValidation();
                    this.tabName = 'QUESTIONNAIRE';
                }, err => {
                    this.isSaving = false;
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Update review failed. Please try again.');
                }));
        }
    }

    clearReview() {
        this.mandatoryList.clear();
        this.reviewDetails = JSON.parse(JSON.stringify(this.tempReviewData));
        this.reviewDetails.deadlineDate = getDateObjectFromTimeStamp(this.reviewDetails.deadlineDate);
    }

    setViewMode() {
        this.isViewMode = this.reviewDetails.extReviewStatus.extReviewStatusCode !== 1 ||
        !this.availableRights.includes('MODIFY_EXT_REVIEW');
    }

    getBadgeByStatusCode() {
        if (this.reviewDetails.extReviewStatus.extReviewStatusCode === 1) {
            return 'info';
        } else if (this.reviewDetails.extReviewStatus.extReviewStatusCode === 2) {
            return 'warning';
        } else {
            return 'success';
        }
    }

    emitSendForReview() {
        this.clearReview();
        this.sendForReview.emit(this.reviewDetails.extReviewID);
    }

    updateUserDetailsFromParent() {
        this.$subscriptions.push(this.emitChildResponse.subscribe((data: any) => {
            if (data && this.reviewDetails.extReviewID === data.extReviewID) {
                this.reviewDetails.extReviewStatus = data.extReviewStatus;
                this.reviewDetails.extReviewStatusCode = data.extReviewStatusCode;
                this.setViewMode();
            }
        }));
    }

    setReviewTypeObj() {
        this.reviewDetails.extReviewServiceType = this.reviewTypes.find(ele =>
            this.reviewDetails.extReviewServiceTypeCode == ele.extReviewServiceTypeCode);
    }

    collapseReview(flag: boolean) {
        this._reviewService.collapseReview = {};
        this._reviewService.collapseReview[this.reviewDetails.extReviewID] = !flag;
    }

    deadlineDateValidation(): void {
        const REQ_DATE = removeTimeZoneFromDateObject(new Date());
        this.deadLineDateValidation = '';
        if (this.reviewDetails.deadlineDate && compareDates(this.reviewDetails.deadlineDate, REQ_DATE) === -1) {
            this.deadLineDateValidation = 'The selected date has already passed.';
        }
    }
}

