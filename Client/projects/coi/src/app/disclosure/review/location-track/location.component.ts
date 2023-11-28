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
import {deepCloneObject, hideModal} from '../../../../../../fibi/src/app/common/utilities/custom-utilities';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../../../../fibi/src/app/app-constants';
import { DATE_PLACEHOLDER } from '../../../../../src/app/app-constants';
import { compareDates, getDateObjectFromTimeStamp, getDuration, parseDateWithoutTimestamp } from '../../../../../../fibi/src/app/common/utilities/date-utilities';
import { PersonProjectOrEntity } from '../../../shared-components/shared-interface';

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
    datePlaceHolder = DATE_PLACEHOLDER;
    personProjectDetails = new PersonProjectOrEntity();

    reviewActionConfirmation: any = {};
    commentConfiguration: CommentConfiguration = new CommentConfiguration();
    projectDetail: any = {};
    isMangeReviewAction = false;
    disReviewLocation = 'COI_REVIEW_LOCATION_TYPE#LOCATION_TYPE_CODE#false#false';
    disReviewStatus = 'COI_REVIEWER_STATUS_TYPE#REVIEW_STATUS_CODE#false#false';
    locationType: any = [];
    reviewerStatusType: any = [];
    reviewStartDate: any;
    reviewEndDate: any;
    collapseViewMore = {};

    constructor(
        private _elasticConfigService: ElasticConfigService,
        private _reviewService: ReviewService,
        private _dataStore: DataStoreService,
        public _commonService: CommonService,
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

    onLocationSelect(event) {
        this.validationMap.delete('location');
        if(event && event.length) {
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
        if(event && event.length) {
            this.reviewDetails.reviewStatusTypeCode = event[0].code;
            this.reviewDetails.reviewerStatusType = {};
            this.reviewDetails.reviewerStatusType['description'] = event[0].description;
            this.reviewDetails.reviewerStatusType['reviewStatusCode'] = event[0].code; 
            if(this.reviewDetails.reviewStatusTypeCode === '2') {
                this.reviewEndDate = new Date();
                this.reviewEndDate.setHours(0, 0, 0, 0);
            }
        } else {
            this.reviewDetails.reviewStatusTypeCode = null;  
            this.reviewDetails.reviewerStatusType = {}; 
        }
    }

    // setStatusLocationType(typeCode, type, event) {
    //     this.reviewDetails[typeCode] = event[0].code;
    //     this.reviewDetails[type] = {};
    //     this.reviewDetails[type]['description'] = event[0].description;
    //     this.reviewDetails[type][typeCode] = event[0].code; 
    // }

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
        this.setPersonProjectDetails();
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
        this.reviewStartDate = getDateObjectFromTimeStamp(this.reviewDetails.startDate);
        if(this.reviewDetails.endDate) {
            this.reviewEndDate = getDateObjectFromTimeStamp(this.reviewDetails.endDate);
        }
        if (this.reviewDetails.reviewLocationType) {
            this.locationType.push({'code': this.reviewDetails.reviewLocationType.locationTypeCode, 'description': this.reviewDetails.reviewLocationType.description});
        } 
        if (this.reviewDetails.reviewerStatusType) {
            this.reviewerStatusType.push({'code': this.reviewDetails.reviewerStatusType.reviewStatusCode, 'description': this.reviewDetails.reviewerStatusType.description});
        }
        this.modifyIndex = index;
        this.personElasticOptions.defaultValue = review.assigneePersonName;
        this.assigneeClearField = new String(false);
        this.adminGroupsCompleterOptions.defaultValue = review.adminGroup ? review.adminGroup.adminGroupName : '';
        this.categoryClearFiled = new String(false);
    }

    saveOrUpdateCoiReview() {
        if (this.validateReview()) {
            this.reviewDetails.disclosureId = this.coiDisclosure.disclosureId;
            this.getReviewDates();
            this.$subscriptions.push(this._reviewService.saveOrUpdateCoiReview({ coiReview: this.reviewDetails }).subscribe((res: any) => {
                this.modifyIndex === -1 ? this.addReviewToList(res) : this.updateReview(res);
                this.reviewDetails = {};
                this._dataStore.updateTimestampEvent.next();
                this.isExpanded = true;
                this._commonService.showToast(HTTP_SUCCESS_STATUS, `Review ${this.modifyIndex === -1 ? 'added' : 'updated'} successfully.`);
                document.getElementById('add-review-modal-trigger').click();
            }, _err => {
                if (_err.status === 405) {
                hideModal('add-coi-reviewer-modal');
                this.coiService.concurrentUpdateAction = 'Create Review';
              } else {
                this._commonService.showToast(HTTP_ERROR_STATUS, `Error in ${this.modifyIndex === -1 ? 'adding' : 'updating'} review.`);
              }
            }));
        }
    }

    getReviewDates() {
        this.reviewDetails.startDate = parseDateWithoutTimestamp(this.reviewStartDate);
        this.reviewDetails.endDate = parseDateWithoutTimestamp(this.reviewEndDate)
    }

    addReviewToList(review: any) {
        this.reviewerList.push(review);
        this.coiDisclosure.coiReviewStatusType = review.coiDisclosure.coiReviewStatusType;
        this._dataStore.updateStore(['coiReviewerList', 'coiDisclosure'], { coiReviewerList: this.reviewerList, coiDisclosure: this.coiDisclosure });        
        this._dataStore.updateTimestampEvent.next();
        this.coiService.isStartReview = this.startReviewIfLoggingPerson() ? true : false;
        this.coiService.isReviewActionCompleted = this.coiService.isAllReviewsCompleted(this.reviewerList);
    }

    updateReview(review: any) {
        this.reviewerList.splice(this.modifyIndex, 1, review);
        this.coiDisclosure.coiReviewStatusType = review.coiDisclosure.coiReviewStatusType;
        this._dataStore.updateStore(['coiReviewerList', 'coiDisclosure'], { coiReviewerList: this.reviewerList, coiDisclosure: this.coiDisclosure });
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
            this.coiDisclosure.coiReviewStatusType = _res.coiReviewStatusType;
            this._dataStore.updateStore(['coiReviewerList' , 'coiDisclosure'], { coiReviewerList: this.reviewerList, coiDisclosure: this.coiDisclosure });
             this.coiService.isReviewActionCompleted = this.coiService.isAllReviewsCompleted(this.reviewerList);
             this.coiService.isStartReview = this.startReviewIfLoggingPerson() ? true : false;
            this._commonService.showToast(HTTP_SUCCESS_STATUS, `Review deleted successfully.`);
            this.clearActionData();
        }, _err => {
            if (_err.status === 405) {
            hideModal('deleteReviewModal');
            this.coiService.concurrentUpdateAction = 'Delete Review';
          } else {
            this.clearActionData();
            this._commonService.showToast(HTTP_ERROR_STATUS, `Error in deleting review.`);
          }
        }));
    }

    modifyReviewComment(coiReviewId) {
        this.commentConfiguration.coiReviewId = coiReviewId;
        this.coiService.isShowCommentNavBar = true;	
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
        if (!this.reviewEndDate && this.reviewDetails.reviewStatusTypeCode == '2') {
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
            const isSelectedReviewer = reviewer.assigneePersonId == this.reviewDetails.assigneePersonId
            return isEditMode ? (isSelectedReviewer && index != this.modifyIndex) : isSelectedReviewer;
        })) {
            this.validationMap.set('reviewer', 'Reviewer already added.');
        }
    }

   
    // Assigned - 1, Completed - 2, In Progress - 3.
    getReviewStatusBadge(statusCode: any) {
        switch (statusCode) {
            case '1': return 'warning';
            case '3': return 'info';
            case '2': return 'success';
            default: return 'danger';
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

    getDaysAtLocation(startDate, endDate) {
        if (startDate) {
            let currentDate = new Date();
            // currentDate.setHours(0, 0, 0, 0);
            return getDuration(getDateObjectFromTimeStamp(startDate), endDate? getDateObjectFromTimeStamp(endDate) : getDateObjectFromTimeStamp(currentDate)).durInDays;
        } else {
            return null;
        }
    }

    collapseViewMoreOption(id: number, flag: boolean): void {
        this.collapseViewMore[id] = !flag;
    }

    private setPersonProjectDetails(): void {
        this.personProjectDetails.personFullName = this.coiDisclosure.person.fullName;
        this.personProjectDetails.projectDetails = this.projectDetail;
        this.personProjectDetails.unitDetails = this.coiDisclosure.person.unit.unitDetail;
    }
}
