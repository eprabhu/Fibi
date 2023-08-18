import {Component, OnDestroy, OnInit} from '@angular/core';
import {Subscription} from 'rxjs';
import {ReviewService} from '../review.service';
import {CommonService} from '../../../common/services/common.service';
import {environment} from '../../../../environments/environment';
import {DataStoreService} from '../../services/data-store.service';
import {AdminGroup, CoiDisclosure, CommentConfiguration, ModalType} from '../../coi-interface';
import {CoiService} from '../../services/coi.service';
import {ElasticConfigService} from '../../../../../../fibi/src/app/common/services/elastic-config.service';
import {subscriptionHandler} from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import {deepCloneObject} from '../../../../../../fibi/src/app/common/utilities/custom-utilities';

@Component({
    selector: 'app-coi-review-location',
    templateUrl: './location.component.html',
    styleUrls: ['./location.component.scss'],
})
export class LocationComponent implements OnInit, OnDestroy {

    $subscriptions: Subscription[] = [];
    dependencies = ['coiDisclosure', 'adminGroup', 'person', 'projectDetail', 'coiReviewerList'];
    coiDisclosure: CoiDisclosure = new CoiDisclosure();
    adminGroups: any = [];
    deployMap = environment.deployUrl;

    isExpanded = true;
    modifyIndex = -1;
    reviewerList: any = [];
    validationMap = new Map();
    disclosurePerson: any = {};

    reviewDetails: any = {};
    personElasticOptions: any = {};
    assigneeClearField: String;
    adminGroupsCompleterOptions: any = {};
    categoryClearFiled: String;

    reviewActionConfirmation: any = {};
    commentConfiguration: CommentConfiguration = new CommentConfiguration();
    projectDetail: any = {};
    isMangeReviewAction = false;

    constructor(
        private _elasticConfigService: ElasticConfigService,
        private _reviewService: ReviewService,
        private _dataStore: DataStoreService,
        private _commonService: CommonService,
        public coiService: CoiService
    ) { }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    ngOnInit() {
        this.isMangeReviewAction = this._commonService.getAvailableRight(['MANAGE_FCOI_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE', 'MANAGE_DISCLOSURE_REVIEW']);
        this.personElasticOptions = this._elasticConfigService.getElasticForPerson();
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                if (dependencies.some((dep) => this.dependencies.includes(dep))) {
                    this.isExpanded = true;
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
        this.projectDetail = DATA.projectDetail;
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
      const DATA = this._dataStore.getData(['coiReviewerList']);
      this.reviewerList = DATA.coiReviewerList || [];
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
                this._dataStore.updateTimestampEvent.next();
                document.getElementById('add-review-modal-trigger').click();
                this.isExpanded = true;
                //this._commonService.showToast(HTTP_SUCCESS_STATUS, `Review ${this.modifyIndex === -1 ? 'added' : 'updated'} successfully.`);
            }, _err => {
                //this._commonService.showToast(HTTP_ERROR_STATUS, `Error in ${this.modifyIndex === -1 ? 'adding' : 'updating'} review.`);
            }));
        }
    }

    addReviewToList(review: any) {
        this.reviewerList.push(review);
        this._dataStore.updateStore(['coiReviewerList'], { coiReviewerList: this.reviewerList });
        this._dataStore.updateTimestampEvent.next();
        this.coiService.isStartReview = this.startReviewIfLoggingPerson() ? true : false;
        this.coiService.isReviewActionCompleted = this.completeReviewAction();
    }

    updateReview(review: any) {
        this.reviewerList.splice(this.modifyIndex, 1, review);
        this._dataStore.updateStore(['coiReviewerList'], { coiReviewerList: this.reviewerList });
        this._dataStore.updateTimestampEvent.next();
        this.coiService.isStartReview = this.startReviewIfLoggingPerson() ? true : false;
    }


    private clearActionData() {
        this.reviewActionConfirmation = {};
        this.modifyIndex = -1;
    }

    deleteReview() {
        this.$subscriptions.push(this._reviewService.deleteReview(this.reviewActionConfirmation.coiReviewId).subscribe((_res: any) => {
            this.reviewerList.splice(this.modifyIndex, 1);
            this._dataStore.updateStore(['coiReviewerList'], { coiReviewerList: this.reviewerList });
             this.coiService.isReviewActionCompleted = this.completeReviewAction();
             this.coiService.isStartReview = this.startReviewIfLoggingPerson() ? true : false;
            //this._commonService.showToast(HTTP_SUCCESS_STATUS, `Review deleted successfully.`);
            this.clearActionData();
        }, _err => {
            this.clearActionData();
            //this._commonService.showToast(HTTP_ERROR_STATUS, `Error in deleting review.`);
        }));
    }

    modifyReviewComment(coiReviewId) {
        this.commentConfiguration.coiReviewId = coiReviewId;
        this.coiService.triggerCommentModal(this.commentConfiguration);
    }

    validateReview() {
        this.validationMap.clear();
        if (!this.reviewDetails.assigneePersonId && !this.reviewDetails.adminGroupId) {
            this.validationMap.set('general', 'Please select a reviewer.');
        }
        if (this.reviewDetails.assigneePersonId) {
            this.isDuplicateReviewerValidation();
        }
        return this.validationMap.size === 0;
    }

    isDuplicateReviewerValidation() {
        const isEditMode = this.modifyIndex != -1;

        if (this.reviewerList.find((reviewer, index) => {
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

  private completeReviewAction (): boolean {
    return this.reviewerList.every(value => value.coiReviewStatus.reviewStatusCode === '4');
  }



  updateCoiReviewStage(index, reviewer, modalType: ModalType) {
    this.modifyIndex = index;
    this.coiService.triggerStartOrCompleteCoiReview(modalType);
    this.coiService.$SelectedReviewerDetails.next(reviewer);
    this.coiService.isEnableReviewActionModal = true;
  }

    private startReviewIfLoggingPerson(): any {
        return this.reviewerList.find(ele =>
            ele.assigneePersonId === this._commonService.currentUserDetails.personId
            && ele.reviewStatusTypeCode === '1');
    }
}
