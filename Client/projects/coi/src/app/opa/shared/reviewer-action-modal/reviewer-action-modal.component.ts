import { Component, OnInit, OnDestroy } from '@angular/core';
import { Subscription } from 'rxjs';
import { DataStoreService } from '../../services/data-store.service';
import { CommonService } from '../../../common/services/common.service';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import {OpaService} from '../../services/opa.service';
import {isEmptyObject} from '../../../../../../fibi/src/app/common/utilities/custom-utilities';
import {OPA} from '../../opa-interface';

@Component({
    selector: 'app-reviewer-action-modal',
    templateUrl: './reviewer-action-modal.component.html',
    styleUrls: ['./reviewer-action-modal.component.scss']
})

export class ReviewerActionModalComponent implements OnInit, OnDestroy {

    currentReviewer: any = {};
    $subscriptions: Subscription[] = [];

    constructor( private _opaService: OpaService,
                 private _dataStore: DataStoreService,
                 private _commonService: CommonService ) { }

    ngOnInit() {
        this.getReviewerDetails();
        document.getElementById(this._opaService.actionButtonId).click();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    getReviewerDetails() {
        this.$subscriptions.push(this._opaService.$SelectedReviewerDetails.subscribe((res: any) => {
            if (!isEmptyObject(res)) {
                this.currentReviewer = res;
            }
        }));
    }

    startCOIReview() {
        this.$subscriptions.push(this._opaService.startReviewerReview(this.currentReviewer.opaReviewId)
            .subscribe((res: any) => {
            this.updateDataStore(res);
            this.currentReviewer = {};
            this._dataStore.updateTimestampEvent.next();
            this._commonService.showToast(HTTP_SUCCESS_STATUS, `Review started successfully.`);
        }, _err => {
            this.currentReviewer = {};
            this._commonService.showToast(HTTP_ERROR_STATUS, `Error in starting review.`);
        }));
    }

    completeReview() {
        this.$subscriptions.push(this._opaService.completeReviewerReview(this.currentReviewer.opaReviewId)
            .subscribe((res: any) => {
            this.updateDataStore(res);
            this.currentReviewer = {};
            this._dataStore.updateTimestampEvent.next();
            this._commonService.showToast(HTTP_SUCCESS_STATUS, `Review completed successfully.`);
        }, _err => {
            this.currentReviewer = {};
            this._commonService.showToast(HTTP_ERROR_STATUS, `Error in completing review.`);
        }));
    }

    closeModal() {
        this._opaService.isEnableReviewActionModal = false;
    }

    updateDataStore(reviewer) {
        const {opaDisclosure, ...review} = reviewer;
        this._opaService.$SelectedReviewerDetails.next(review);
        const DATA: OPA = this._dataStore.getData();
        const reviewerList = DATA.opaReviewerList || [];
        const index = reviewerList.findIndex(ele => ele.opaReviewId === reviewer.opaReviewId);
        reviewerList[index] = reviewer;
        DATA.opaDisclosure.opaDisclosureStatusCode = opaDisclosure.opaDisclosureStatusCode;
        DATA.opaDisclosure.opaDisclosureStatusType = opaDisclosure.opaDisclosureStatusType;
        this._dataStore.updateStore(['opaReviewerList', 'opaDisclosure'],
            { opaReviewerList: reviewerList, opaDisclosure: DATA.opaDisclosure });
        this._opaService.isReviewActionCompleted = this._opaService.isAllReviewsCompleted(reviewerList);
        this.updateReviewActions(reviewer);
        this._opaService.isEnableReviewActionModal = false;
    }

    updateReviewActions(reviewer) {
        this._opaService.isDisclosureReviewer = reviewer.assigneePersonId === this._commonService.currentUserDetails.personId;
        if (reviewer.reviewStatusTypeCode === '2' && this._opaService.isDisclosureReviewer) {
            this._opaService.isStartReview = false;
            this._opaService.isCompleteReview = true;
        } else if (reviewer.reviewStatusTypeCode === '3' && this._opaService.isDisclosureReviewer) {
            this._opaService.isStartReview = false;
            this._opaService.isCompleteReview = false;
        }
    }

}
