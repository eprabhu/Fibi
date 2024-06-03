import {Component, OnDestroy, OnInit} from '@angular/core';
import {Subscription} from 'rxjs';
import {ReviewService} from '../review.service';
import {CommonService} from '../../../common/services/common.service';
import {environment} from '../../../../environments/environment';
import {DataStoreService, StoreData} from '../../services/data-store.service';
import {subscriptionHandler} from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import {deepCloneObject} from '../../../../../../fibi/src/app/common/utilities/custom-utilities';
import {HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS} from '../../../../../../fibi/src/app/app-constants';
import {DATE_PLACEHOLDER} from '../../../app-constants';
import {
    compareDates,
    getDateObjectFromTimeStamp,
    getDuration,
    parseDateWithoutTimestamp
} from '../../../../../../fibi/src/app/common/utilities/date-utilities';
import {PersonProjectOrEntity, coiReviewComment} from '../../../shared-components/shared-interface';
import {CommentConfiguration, ModalType} from '../../../disclosure/coi-interface';
import {OpaService} from '../../services/opa.service';
import {OPA, OpaDisclosure} from '../../opa-interface';
import { ElasticConfigService } from '../../../common/services/elastic-config.service';

@Component({
    selector: 'app-coi-review-location',
    templateUrl: './location.component.html',
    styleUrls: ['./location.component.scss'],
})
export class LocationComponent implements OnInit, OnDestroy {

    $subscriptions: Subscription[] = [];
    opaDisclosure: OpaDisclosure = new OpaDisclosure();
    deployMap = environment.deployUrl;

    isExpanded = true;
    modifyIndex = -1;
    reviewerList: any = [];
    validationMap = new Map();

    reviewDetails: any = {};
    personElasticOptions: any = {};
    assigneeClearField: String;
    categoryClearFiled: String;
    datePlaceHolder = DATE_PLACEHOLDER;
    personProjectDetails = new PersonProjectOrEntity();

    reviewActionConfirmation: any = {};
    commentConfiguration: CommentConfiguration = new CommentConfiguration();
    projectDetail: any = {};
    isMangeReviewAction = false;
    disReviewLocation = 'OPA_REVIEW_LOCATION_TYPE#LOCATION_TYPE_CODE#false#false';
    disReviewStatus = 'OPA_REVIEW_REVIEWER_STATUS_TYPE#REVIEW_STATUS_CODE#false#false';
    locationType: any = [];
    reviewStatusType: any = [];
    reviewStartDate: any;
    reviewEndDate: any;
    collapseViewMore = {};
    isCOIAdministrator = false;
    isOPAEditMode = false;
    editReviewerHelpText = 'You are about to edit the review.';
    addReviewerHelpText = 'You are about to add a review.';
    deleteReviewHelpText = 'You are about to delete the review.'

    constructor(
        private _elasticConfigService: ElasticConfigService,
        private _reviewService: ReviewService,
        private _dataStore: DataStoreService,
        public _commonService: CommonService,
        public opaService: OpaService
    ) {
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    ngOnInit() {
        // this.isMangeReviewAction = this._commonService.getAvailableRight(['OPA_ADMINISTRATOR']);
        this.isMangeReviewAction = this._commonService.getAvailableRight(['MANAGE_OPA_DISCLOSURE']);
        this.personElasticOptions = this._elasticConfigService.getElasticForPerson();
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    onLocationSelect(event) {
        this.validationMap.delete('location');
        if (event && event.length) {
            this.reviewDetails.locationTypeCode = event[0].code;
            this.reviewDetails.reviewLocationType = {};
            this.reviewDetails.reviewLocationType['description'] = event[0].description;
            this.reviewDetails.reviewLocationType['locationTypeCode'] = event[0].code;
        } else {
            this.reviewDetails.locationTypeCode = null;
            this.reviewDetails.reviewLocationType = {};
        }
    }

    onStatusSelect(event) {
        this.reviewEndDate = '';
        this.validationMap.delete('reviewEndDate');
        this.validationMap.delete('reviewStatus');
        if (event && event.length) {
            this.reviewDetails.reviewStatusTypeCode = event[0].code;
            this.reviewDetails.reviewStatusType = {};
            this.reviewDetails.reviewStatusType['description'] = event[0].description;
            this.reviewDetails.reviewStatusType['reviewStatusCode'] = event[0].code;
            if (this.reviewDetails.reviewStatusTypeCode === '3') {
                this.reviewEndDate = new Date();
                this.reviewEndDate.setHours(0, 0, 0, 0);
            }
        } else {
            this.reviewDetails.reviewStatusTypeCode = null;
            this.reviewDetails.reviewStatusType = {};
        }
    }

    getCoiReview() {
        this.$subscriptions.push(this._reviewService.getCoiReview(this.opaDisclosure.opaDisclosureId).subscribe((res) => {
            this.reviewerList = res || [];
        }, err => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching review details.');
        }));
    }

    assigneeSelect(event: any): void {
        this.reviewDetails.assigneePersonId = event ? event.prncpl_id : null;
        this.reviewDetails.assigneePersonName = event ? event.full_name : null;
    }

    editReview(review: any, index: number): void {
        this.clearReviewModal();
        this.reviewDetails = deepCloneObject(review);
        this.reviewDetails.currentReviewStatusTypeCode = this.reviewDetails.reviewStatusType.reviewStatusCode;
        this.reviewDetails.currentLocationTypeCode = this.reviewDetails.reviewLocationType.locationTypeCode;
        this.reviewStartDate = getDateObjectFromTimeStamp(this.reviewDetails.startDate);
        if (this.reviewDetails.endDate) {
            this.reviewEndDate = getDateObjectFromTimeStamp(this.reviewDetails.endDate);
        }
        if (this.reviewDetails.reviewLocationType) {
            this.locationType.push({
                'code': this.reviewDetails.reviewLocationType.locationTypeCode,
                'description': this.reviewDetails.reviewLocationType.description
            });
        }
        if (this.reviewDetails.reviewStatusType) {
            this.reviewStatusType.push({
                'code': this.reviewDetails.reviewStatusType.reviewStatusCode,
                'description': this.reviewDetails.reviewStatusType.description
            });
        }
        this.modifyIndex = index;
        this.personElasticOptions.defaultValue = review.assigneePersonName;
        this.assigneeClearField = new String(false);
        this.categoryClearFiled = new String(false);
    }

    saveOrUpdateCoiReview() {
        if (this.validateReview()) {
            this.reviewDetails.opaDisclosureId = this.opaDisclosure.opaDisclosureId;
            this.getReviewDates();
            this.$subscriptions.push(this._reviewService.saveOrUpdateCoiReview(this.reviewDetails).subscribe((res: any) => {
                this._commonService.showToast(HTTP_SUCCESS_STATUS, `Reviewer ${this.modifyIndex === -1 ? 'added' : 'updated'} successfully.`);
                this.opaDisclosure.updateTimestamp = res.opaDisclosure.updateTimestamp;
                this.opaDisclosure.updateUserFullName = res.updateUserFullName;
                this.modifyIndex === -1 ? this.addReviewToList(res) : this.updateReview(res);
                this.reviewDetails = {};
                document.getElementById('add-review-modal-trigger').click();
                this.isExpanded = true;
            }, _err => {
                if (_err.status === 405) {
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Action you are trying to perform is not valid for current state, please refresh.');
                  } else {
                    this._commonService.showToast(HTTP_ERROR_STATUS, typeof(_err.error) == 'string' ? _err.error : `Error in ${this.modifyIndex === -1 ? 'adding' : 'updating'} review.`);
                }
            }));
        }
    }

    getReviewDates() {
        this.reviewDetails.startDate = parseDateWithoutTimestamp(this.reviewStartDate);
        this.reviewDetails.endDate = parseDateWithoutTimestamp(this.reviewEndDate);
    }

    private setReviewAndOPAData(review: any) {
        const {opaDisclosure, ...reviewer} = review;
        this.opaDisclosure.reviewStatusType  = opaDisclosure.reviewStatusType;
        this.opaDisclosure.reviewStatusCode  = opaDisclosure.reviewStatusCode;
        return reviewer;
    }

    addReviewToList(review: any) {
        const reviewer = this.setReviewAndOPAData(review);
        this.reviewerList.push(reviewer);
        this.updateReviewDetails();
        this.opaService.isReviewActionCompleted = this.opaService.isAllReviewsCompleted(this.reviewerList);
    }

    updateReview(review: any) {
        const reviewer = this.setReviewAndOPAData(review);
        this.reviewerList.splice(this.modifyIndex, 1, reviewer);
        this.updateReviewDetails();
    }

    updateReviewDetails() {
        this._dataStore.updateStore(['opaReviewerList', 'opaDisclosure'], {
            opaReviewerList: this.reviewerList,
            opaDisclosure: this.opaDisclosure
        });
        this.startOrCompleteReview();
    }

    deleteReview() {
        this.$subscriptions.push(this._reviewService.deleteReview(this.reviewActionConfirmation.opaReviewId).subscribe((res: any) => {
            this.reviewerList.splice(this.modifyIndex, 1);
            this.opaDisclosure.updateTimestamp = res.updateTimestamp;
            this.opaDisclosure.updateUserFullName = res.updateUserFullName;
            this.setReviewAndOPAData(res);
            this._dataStore.updateStore(['opaReviewerList', 'opaDisclosure'], {
                opaReviewerList: this.reviewerList,
                opaDisclosure: this.opaDisclosure
            });
            this.opaService.isReviewActionCompleted = this.opaService.isAllReviewsCompleted(this.reviewerList);
            this.startOrCompleteReview();
            this._commonService.showToast(HTTP_SUCCESS_STATUS, `Review deleted successfully.`);
            this.clearActionData();
        }, _err => {
            if (_err.status === 405) {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Action you are trying to perform is not valid for current state, please refresh.');
            } else {
                this.clearActionData();
                this._commonService.showToast(HTTP_ERROR_STATUS, `Error in deleting review.`);
            }
        }));
    }

    modifyReviewComment(reviewDetails) {
        const disclosureDetails:coiReviewComment = {
            documentOwnerPersonId: this.opaDisclosure.personId,
            componentTypeCode: '11',
            subModuleItemKey: reviewDetails.opaReviewId,
            coiSubSectionsTitle: 'Review comments at ' + reviewDetails.reviewLocationType.description
        }
        this._commonService.$commentConfigurationDetails.next(disclosureDetails);
        this.opaService.isShowCommentNavBar = true;
    }

    validateReview() {
        this.validationMap.clear();
        this.isEndDateInValid();
        if (this.reviewDetails.assigneePersonId) {
            this.isDuplicateReviewerValidation();
        }
        if (!this.reviewDetails.locationTypeCode) {
            this.validationMap.set('location', 'Please select a location.');
        }
        if (!this.reviewStartDate) {
            this.validationMap.set('reviewStartDate', 'Please select a review start date.');
        }
        if (!this.reviewEndDate && this.reviewDetails.reviewStatusTypeCode == '3') {
            this.validationMap.set('reviewEndDate', 'Please select a review end date.');
        }
        if (!this.reviewDetails.reviewStatusTypeCode) {
            this.validationMap.set('reviewStatus', 'Please select a status.');
        }
        return this.validationMap.size === 0;
    }

    isEndDateInValid() {
        if (this.reviewStartDate && this.reviewEndDate &&
            (compareDates(this.reviewStartDate, this.reviewEndDate) === 1)) {
            this.validationMap.set('endDate', 'Please provide a valid review end date.');
        }
    }

    compareDates(type: any) {
        this.validationMap.delete('endDate');
        type === 'START' ? this.validationMap.delete('reviewStartDate') : this.validationMap.delete('reviewEndDate');
        this.isEndDateInValid();
    }

    isDuplicateReviewerValidation() {
        const isEditMode = this.modifyIndex != -1;
        if (this.reviewerList.find((reviewer, index) => {
            const isSelectedReviewer = (reviewer.assigneePersonId == this.reviewDetails.assigneePersonId && reviewer.reviewStatusTypeCode != '3' && this.reviewDetails.locationTypeCode == reviewer.locationTypeCode);
            return isEditMode ? (isSelectedReviewer && index != this.modifyIndex) : isSelectedReviewer;
        })) {
            this.assigneeClearField = new String('true');
            this.validationMap.set('reviewer', 'Reviewer already added.');
        }
    }

    // Assigned - 1, Completed - 2, In Progress - 3.
    getReviewStatusBadge(statusCode: any) {
        switch (statusCode) {
            case '1':
                return 'warning';
            case '2':
                return 'info';
            case '3':
                return 'success';
            default:
                return 'danger';
        }
    }

    clearReviewModal() {
        this.reviewDetails = {};
        this.modifyIndex = -1;
        this.validationMap.clear();
        this.assigneeClearField = new String('true');
        this.categoryClearFiled = new String('true');
        this.reviewStartDate = new Date();
        this.reviewStartDate.setHours(0, 0, 0, 0);
        this.reviewEndDate = '';
        this.locationType = [];
        this.reviewStatusType = [];
    }

    updateCoiReviewStage(index, reviewer, modalType: ModalType) {
        this.modifyIndex = index;
        this.opaService.triggerStartOrCompleteCoiReview(modalType);
        this.opaService.$SelectedReviewerDetails.next(reviewer);
        this.opaService.isEnableReviewActionModal = true;
    }

    getDaysAtLocation(startDate, endDate) {
        if (startDate) {
            const currentDate = new Date();
            return getDuration(getDateObjectFromTimeStamp(startDate), endDate? getDateObjectFromTimeStamp(endDate) : getDateObjectFromTimeStamp(currentDate)).durInDays;
        } else {
            return null;
        }
    }

    collapseViewMoreOption(id: number, flag: boolean): void {
        this.collapseViewMore[id] = !flag;
    }

    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.isExpanded = true;
                this.getDataFromStore();
            })
        );
    }

    // Returned - 5, Pending - 1, Withdrwan - 6
    private getDataFromStore() {
        const DATA: OPA = this._dataStore.getData();
        this.opaDisclosure = DATA.opaDisclosure;
        this.isOPAEditMode = ['1', '5', '6'].includes(DATA.opaDisclosure.reviewStatusCode);
        this.commentConfiguration.disclosureId = this.opaDisclosure.opaDisclosureId;
        this.isCOIAdministrator = this._commonService.getAvailableRight(['OPA_ADMINISTRATOR', 'VIEW_ADMIN_GROUP_OPA']);
        this.getCoiReview();
        this.setPersonProjectDetails();
    }

    private clearActionData() {
        this.reviewActionConfirmation = {};
        this.modifyIndex = -1;
    }

    private startReviewIfLoggingPerson(): any {
        return this.reviewerList.find(ele =>
            ele.assigneePersonId === this._commonService.currentUserDetails.personId
            && ele.reviewStatusTypeCode === '1');
    }

    startOrCompleteReview() {
        this.opaService.isStartReview = false;
        this.opaService.isCompleteReview = false;
        let nextAssignedReview = this.getNextAssignedReview();
        if (nextAssignedReview) {
            this.opaService.currentOPAReviewForAction = nextAssignedReview;
            if(nextAssignedReview.reviewStatusTypeCode == 1)
                this.opaService.isStartReview = true;
            else if (nextAssignedReview.reviewStatusTypeCode == 2)
                this.opaService.isCompleteReview = true;
        }

    }

    private getNextAssignedReview(): any {
        return this.reviewerList.find(ele =>
            ele.assigneePersonId === this._commonService.currentUserDetails.personId
            && ele.reviewStatusTypeCode !== '3');
    }

    private setPersonProjectDetails(): void {
        this.personProjectDetails.personFullName = this.opaDisclosure.opaPerson.personName;
        this.personProjectDetails.projectDetails = this.projectDetail;
        this.personProjectDetails.homeUnit = this.opaDisclosure.homeUnit;
        this.personProjectDetails.homeUnitName = this.opaDisclosure.homeUnitName;
    }
}
