import {Component, OnDestroy, OnInit} from '@angular/core';
import {Subscription} from 'rxjs';
import {ReviewService} from '../review.service';
import {CommonService} from '../../../common/services/common.service';
import {environment} from '../../../../environments/environment';
import {DataStoreService} from '../../services/data-store.service';
import {AdminGroup, CoiDisclosure, CommentConfiguration} from '../../coi-interface';
import {CoiService} from '../../services/coi.service';
import {ElasticConfigService} from "../../../../../../fibi/src/app/common/services/elastic-config.service";
import {subscriptionHandler} from "../../../../../../fibi/src/app/common/utilities/subscription-handler";
import {deepCloneObject} from "../../../../../../fibi/src/app/common/utilities/custom-utilities";

declare var $: any;

@Component({
    selector: 'app-coi-review-location',
    templateUrl: './location.component.html',
    styleUrls: ['./location.component.css'],
})
export class LocationComponent implements OnInit, OnDestroy {

    $subscriptions: Subscription[] = [];
    dependencies = ['coiDisclosure', 'adminGroup', 'person'];
    coiDisclosure: CoiDisclosure = new CoiDisclosure();
    adminGroups: any = [];
    deployMap = environment.deployUrl;

    isCollapsed = true;
    modifyIndex = -1;
    reviewList: any = [];
    validationMap = new Map();
    disclosurePerson: any = {};

    reviewDetails: any = {};
    personElasticOptions: any = {};
    assigneeClearField: String;
    adminGroupsCompleterOptions: any = {};
    categoryClearFiled: String;

    reviewActionConfirmation: any = {};
    commentConfiguration: CommentConfiguration = new CommentConfiguration();

    constructor(
        private _elasticConfigService: ElasticConfigService,
        private _reviewService: ReviewService,
        private _dataStore: DataStoreService,
        private _commonService: CommonService,
        private _coiService: CoiService
    ) { }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    ngOnInit() {
        this.personElasticOptions = this._elasticConfigService.getElasticForPerson();
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                if (dependencies.some((dep) => this.dependencies.includes(dep))) {
                    this.getDataFromStore();
                }
            })
        );
    }

    private getDataFromStore() {
        const DATA = this._dataStore.getData(this.dependencies);
        this.coiDisclosure = DATA.coiDisclosure;
        this.adminGroups = DATA.adminGroup || [];
        this.disclosurePerson = DATA.person;
        this.commentConfiguration.disclosureId = this.coiDisclosure.disclosureId;
        this.getCoiReview();
        this.setAdminGroupOptions();
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

    private getActiveAdminGroups(): AdminGroup[] {
        return this.adminGroups.filter(element => element.isActive === 'Y');
    }

    getCoiReview() {
        this.$subscriptions.push(this._reviewService.getCoiReview(this.coiDisclosure.disclosureId).subscribe((res: any) => {
            this.reviewList = res;
        }, _err => {
           // this._commonService.showToast(HTTP_ERROR_STATUS, `Error in ${this.modifyIndex === -1 ? 'adding' : 'updating'} review.`);
        }));
    }

    adminGroupSelect(event: any): void {
        this.reviewDetails.adminGroupId = event ? event.adminGroupId : null;
        this.reviewDetails.adminGroup = event ? event : null;
    }

    assigneeSelect(event: any): void {
        this.reviewDetails.assigneePersonId = event ? event.prncpl_id : null;
        this.reviewDetails.assigneePersonName = event ? event.full_name : null;
    }

    editReview(review: any, index: number): void {
        this.clearReviewModal();
        this.reviewDetails = deepCloneObject(review);
        this.modifyIndex = index;
        this.personElasticOptions.defaultValue = review.assigneePersonName;
        this.assigneeClearField = new String(false);
        this.adminGroupsCompleterOptions.defaultValue = review.adminGroup ? review.adminGroup.adminGroupName : '';
        this.categoryClearFiled = new String(false);
    }

    saveOrUpdateCoiReview() {
        if (this.validateReview()) {
            this.reviewDetails.disclosureId = this.coiDisclosure.disclosureId;
            this.$subscriptions.push(this._reviewService.saveOrUpdateCoiReview({ coiReview: this.reviewDetails }).subscribe((res: any) => {
                this.modifyIndex === -1 ? this.addReviewToList(res) : this.updateReview(res);
                this.modifyIndex = -1;
                this.reviewDetails = {};
                document.getElementById('add-review-modal-trigger').click();
                //this._commonService.showToast(HTTP_SUCCESS_STATUS, `Review ${this.modifyIndex === -1 ? 'added' : 'updated'} successfully.`);
            }, _err => {
                //this._commonService.showToast(HTTP_ERROR_STATUS, `Error in ${this.modifyIndex === -1 ? 'adding' : 'updating'} review.`);
            }));
        }
    }

    addReviewToList(review: any) {
        this.reviewList.push(review);
    }

    updateReview(review: any) {
        this.reviewList.splice(this.modifyIndex, 1, review);
    }

    startCOIReview() {
        this.$subscriptions.push(this._reviewService.startCOIReview({
            coiReviewId: this.reviewActionConfirmation.coiReviewId,
            assigneePersonName: this.reviewActionConfirmation.assigneePersonName
        }).subscribe((res: any) => {
            this.updateReview(res);
            //this._commonService.showToast(HTTP_SUCCESS_STATUS, `Review started successfully.`);
            this.clearActionData();
        }, _err => {
            this.clearActionData();
            //this._commonService.showToast(HTTP_ERROR_STATUS, `Error in starting review.`);
        }));
    }

    completeReview() {
        this.$subscriptions.push(this._reviewService.completeReview({
            coiReviewId: this.reviewActionConfirmation.coiReviewId,
            assigneePersonName: this.reviewActionConfirmation.assigneePersonName
        }).subscribe((res: any) => {
            this.updateReview(res);
            //this._commonService.showToast(HTTP_SUCCESS_STATUS, `Review completed successfully.`);
            this.clearActionData();
        }, _err => {
            this.clearActionData();
            //this._commonService.showToast(HTTP_ERROR_STATUS, `Error in completing review.`);
        }));
    }

    private clearActionData() {
        this.reviewActionConfirmation = {};
        this.modifyIndex = -1;
    }

    deleteReview() {
        this.$subscriptions.push(this._reviewService.deleteReview(this.reviewActionConfirmation.coiReviewId).subscribe((_res: any) => {
            this.reviewList.splice(this.modifyIndex, 1);
            //this._commonService.showToast(HTTP_SUCCESS_STATUS, `Review deleted successfully.`);
            this.clearActionData();
        }, _err => {
            this.clearActionData();
            //this._commonService.showToast(HTTP_ERROR_STATUS, `Error in deleting review.`);
        }));
    }

    modifyReviewComment(coiReviewId) {
        this.commentConfiguration.coiReviewId = coiReviewId;
        this._coiService.triggerCommentModal(this.commentConfiguration);
    }

    validateReview() {
        this.validationMap.clear();
        if (!this.reviewDetails.assigneePersonId && !this.reviewDetails.adminGroupId) {
            this.validationMap.set('general', 'Please select an admin group or assignee.');
        }
        if (this.reviewDetails.assigneePersonId) {
            this.isDuplicateReviewerValidation();
        }
        return this.validationMap.size === 0;
    }

    isDuplicateReviewerValidation() {
        const isEditMode = this.modifyIndex != -1;

        if (this.reviewList.find((reviewer, index) => {
            const isSelectedReviewer = reviewer.assigneePersonId == this.reviewDetails.assigneePersonId
            return isEditMode ? (isSelectedReviewer && index != this.modifyIndex) : isSelectedReviewer;
        })) {
            this.validationMap.set('reviewer', 'Reviewer already added.');
        }
    }

    getReviewStatusBadge(statusCode: any) {
        switch (statusCode) {
            case '1': return 'warning';
            case '2': return 'info';
            case '3': return 'info';
            case '5': return 'warning';
            case '4': return 'success';
            default: return 'danger';
        }
    }

    clearReviewModal() {
        this.reviewDetails = {};
        this.modifyIndex = -1;
        this.validationMap.clear();
        this.assigneeClearField = new String('true');
        this.categoryClearFiled = new String('true');
    }
}
