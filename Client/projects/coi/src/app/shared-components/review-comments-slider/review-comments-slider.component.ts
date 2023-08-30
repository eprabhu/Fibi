import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { closeSlider, openSlider } from '../../common/utilities/custom-utilities';
import { ReviewComments } from './review-comments-interface';
import { ReviewCommentsService } from './review-comments.service';
import { CompleterOptions } from '../../../../../fibi/src/app/service-request/service-request.interface';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { topSlideInOut } from '../../common/utilities/animations';


@Component({
    selector: 'app-review-comments-slider',
    templateUrl: './review-comments-slider.component.html',
    styleUrls: ['./review-comments-slider.component.scss'],
    providers: [ReviewCommentsService],
    animations: [topSlideInOut]
})
export class ReviewCommentsSliderComponent implements OnInit, OnDestroy, OnChanges {

    headerName = ''
    @Output() closePage: EventEmitter<any> = new EventEmitter<any>();

    isSaving = false;
    $subscriptions: Subscription[] = [];
    isReadMore = false;
    reviewCommentDetails: ReviewComments = new ReviewComments();
    reviewTypeList: any;
    adminGroupsCompleterOptions: CompleterOptions = new CompleterOptions();
    clearAdminGroupField: any;
    assignAdminMap = new Map();
    adminSearchOptions: any = {};
    clearAdministratorField: String;
    isAddAssignee = false;
    selectedReviewType: any = {};
    commentList: any = [];
    uploadedFile: any = [];
    mandatoryMap = new Map();
    isEditComment = false;
    isReplyComment = false;
    isReplayCommentReadMore = false;
    isChangesInField = false;
    isAddAttachment = false;
    isCloseSlider = false;

    constructor(
        private _commonService: CommonService, private _reviewCommentsService: ReviewCommentsService
    ) { }

    ngOnInit() {
        this.getReviewerActionDetails();
        this.loadSectionsTypeCode();
        this.getAdminDetails();
        setTimeout(() => {
            openSlider('review-comments-slider');
        });
    }

    ngOnChanges() {
        // if(this.requestLoadCommentDetails) {
        //     this.getCoiReviewComments(this.requestLoadCommentDetails);
        // }
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    openConformationModal() {
        document.getElementById('review-comments-confirmation-modal-trigger-btn').click();
    }

    validateSliderClose() {
        this.isCloseSlider = true;
        (this.isChangesInField) ? this.openConformationModal() : this.closeReviewSlider();
    }

    closeReviewSlider() {
        closeSlider('review-comments-slider');
        setTimeout(() => {
            this.cancelOrClearCommentsDetails();
            this.closePage.emit(false);
        }, 500);
    }

    leavePageClicked(event: boolean = false) {
        if (!event) {
            setTimeout(() => {
                this.closeReviewSlider();
            }, 100);
        }
    }

    loadSectionsTypeCode() {
        this.$subscriptions.push(this._reviewCommentsService.getSectionsTypeCode().subscribe(res => {
            this.reviewTypeList = res;
            this.selectedReviewType = this.reviewTypeList.find(ele => ele.coiSectionsTypeCode === this.reviewCommentDetails.coiReviewCommentDto.coiSectionsTypeCode);
        }));
    }

    fileDrop(event) {
        if (event) {
            const attachment = {
                fileName: event[0].name,
                commentId: null,
                attachmentId: null,
                mimeType: event[0].type,
            }
            this.reviewCommentDetails.coiReviewCommentDto.coiReviewCommentAttachment.push(attachment);
            this.uploadedFile.push(event[0]);
            this.isChangesInField = true;
        }
    }

    public adminSelect(event: any) {
        if (event) {
            if (!this.validateAdmin(event, 'ADMINISTRATOR')) {
                const adminPersonDetails = {
                    tagPersonId: event.personId,
                    tagPersonFullName: event.fullName,
                    tagGroupId: null,
                    tagGroupName: '',
                }
                this.reviewCommentDetails.coiReviewCommentDto.coiReviewCommentTag.push(adminPersonDetails);
                this.assignAdminMap.clear();
                this.clearAdministratorField = new String('true');
                this.isChangesInField = true;
            }

        }
    }

    public adminGroupSelect(event) {
        if (event) {
            if (!this.validateAdmin(event, 'ADMIN_GROUP')) {
                const adminGroupDetails = {
                    tagPersonId: '',
                    tagPersonFullName: '',
                    tagGroupId: event.adminGroupId,
                    tagGroupName: event.adminGroupName,
                }
                this.reviewCommentDetails.coiReviewCommentDto.coiReviewCommentTag.push(adminGroupDetails);
                this.assignAdminMap.clear();
                this.clearAdminGroupField = new String('true');
                this.isChangesInField = true;
            }
        }
    }

    private getAdminDetails() {
        this.$subscriptions.push(this._reviewCommentsService.getAdminDetails().subscribe((data: any) => {
            this.setAdminGroupOptions(data);
            this.setCompleterOptions(data.persons, 'fullName', this.adminSearchOptions);
        }));
    }

    private setAdminGroupOptions(data): void {
        this.adminGroupsCompleterOptions = {
            arrayList: this.getActiveAdminGroups(data),
            contextField: 'adminGroupName',
            filterFields: 'adminGroupName',
            formatString: 'adminGroupName',
            defaultValue: ''
        };
    }


    private getActiveAdminGroups(data) {
        return data.adminGroups.filter(element => element.isActive === 'Y');
    }

    private setCompleterOptions(arrayList: any, searchShowField: string, searchOption: any = null) {
        searchOption.defaultValue = '';
        searchOption.arrayList = arrayList || [];
        searchOption.contextField = searchShowField;
        searchOption.filterFields = searchShowField;
        searchOption.formatString = searchShowField;
    }


    deleteCoiCommentAssignee(tagDetails, index) {
        if (this.isEditComment && tagDetails.coiReviewCommentTagsId) {
            this.$subscriptions.push(this._reviewCommentsService.deleteCOIAssignee(tagDetails.coiReviewCommentTagsId).subscribe(res => {
                this.reviewCommentDetails.coiReviewCommentDto.coiReviewCommentTag.splice(index, 1);
                this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Assignee removed successfully.')
            }, err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.')
            }));
        } else {
            this.reviewCommentDetails.coiReviewCommentDto.coiReviewCommentTag.splice(index, 1);
        }
    }

    addCommentsDetails() {
        if (!this.validateComment()) {
            this.addOrUpdateComment(this.reviewCommentDetails, this.uploadedFile);
        }
    }


    private addOrUpdateComment(details, file) {
        this.$subscriptions.push(this._reviewCommentsService.addCOIReviewComment(details, file).subscribe((res: any) => {
            this.getCoiReviewComments(this.loadReviewerCommentBody(res));
            this.cancelOrClearCommentsDetails();
            this._commonService.showToast(HTTP_SUCCESS_STATUS, 'General review comment added Successfully');
        }, error => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }

    private validateAdmin(event, type): boolean {
        this.assignAdminMap.clear();
        if (type === 'ADMINISTRATOR' && this.checkAlreadyAdd(event, type)) {
            this.assignAdminMap.set('adminName', 'Administrator already been added');
        }
        if (type === 'ADMIN_GROUP' && this.checkAlreadyAdd(event, type)) {
            this.assignAdminMap.set('adminGroup', 'Admin Group already been added');
        }
        return this.assignAdminMap.size === 0 ? false : true;
    }

    checkAlreadyAdd(event, type) {
        return this.reviewCommentDetails.coiReviewCommentDto.coiReviewCommentTag.some((element) =>
            type === 'ADMINISTRATOR' ? element.tagPersonId === event.personId : element.tagGroupId === event.adminGroupId);
    }

    getCoiReviewComments(REQ_BODY) {
        this.$subscriptions.push(this._reviewCommentsService.getCoiReviewComments(REQ_BODY).subscribe((res: any) => {
            this.commentList = res;
            if (this._reviewCommentsService.editParentCommentId) {
                this._reviewCommentsService.editParentCommentId = null;
            }
        }));
    }

    getReviewerActionDetails() {
        this.$subscriptions.push(this._commonService.$commentConfigurationDetails.subscribe((res: any) => {
            this.reviewCommentDetails.documentOwnerPersonId = res.documentOwnerPersonId;
            this.reviewCommentDetails.coiReviewCommentDto.disclosureId = res.disclosureId;
            this.reviewCommentDetails.coiReviewCommentDto.coiSectionsTypeCode = res.coiSectionsTypeCode;
            if (this.reviewCommentDetails.coiReviewCommentDto.coiSectionsTypeCode === '5' || this.reviewCommentDetails.coiReviewCommentDto.coiSectionsTypeCode === '6') {
                this.headerName = res.headerName;
                this.reviewCommentDetails.coiReviewCommentDto.coiSubSectionsId = res.coiSubSectionsId;
                this.reviewCommentDetails.coiReviewCommentDto.componentSubRefId = res.componentSubRefId;

            }
            this.getCoiReviewComments(this.loadReviewerCommentBody(res));
        }));
    }

    deleteCoiCommentAttachments(item, index) {
        if (this.isEditComment) {
            this.$subscriptions.push(this._reviewCommentsService.deleteCOICommentAttachment({ attachmentId: item.attachmentId, fileDataId: item.fileDataId }).subscribe(res => {
                this.reviewCommentDetails.coiReviewCommentDto.coiReviewCommentAttachment.splice(index, 1);
                this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Attachment deleted successfully');
            }, error => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
            }));
        } else {
            this.uploadedFile.splice(index, 1);
        }
    }

    validateComment() {
        this.mandatoryMap.clear();
        if (this.reviewCommentDetails.coiReviewCommentDto.comment === '') {
            this.mandatoryMap.set('comment', 'Please add comment');
        }
        return this.mandatoryMap.size === 0 ? false : true;
    }



    cancelOrClearCommentsDetails() {
        this.reviewCommentDetails = new ReviewComments();
        this.uploadedFile = [];
        if (this.isEditComment) {
            this.isEditComment = false;
        }
        if (this._reviewCommentsService.isEditParentComment) {
            this._reviewCommentsService.isEditParentComment = false;
        }
        this.isChangesInField = false;
        if (!this.isCloseSlider) {
            this.getReviewerActionDetails();
        }
        if (this._reviewCommentsService.editParentCommentId) {
            this._reviewCommentsService.editParentCommentId = null;
        }
    }

    deleteReviewComment(event) {
        this.commentList.splice(event, 1);
    }

    editReviewerParentComment(event) {
        this.isEditComment = true;
        this.reviewCommentDetails.documentOwnerPersonId = event.documentOwnerPersonId;
        this.reviewCommentDetails.coiReviewCommentDto.commentId = event.commentId;
        this.reviewCommentDetails.coiReviewCommentDto.isPrivate = event.isPrivate;
        this.reviewCommentDetails.coiReviewCommentDto.comment = event.comment;
        this.reviewCommentDetails.coiReviewCommentDto.coiSectionsTypeCode = event.disclCommentType.commentType;
        this.reviewCommentDetails.coiReviewCommentDto.disclosureId = this.reviewCommentDetails.coiReviewCommentDto.disclosureId;
        this.updateAttachmentDetails(event.disclAttachments);
        this.updateAssigneeDetails(event.coiReviewCommentTag);

    }

    private updateAssigneeDetails(assigneeDetails: any) {
        assigneeDetails.forEach(ele => {
            const reviewerList: any = {};
            reviewerList.tagPersonId = ele.tagPersonId;
            reviewerList.tagPersonFullName = ele.tagPersonFullName;
            reviewerList.tagGroupId = ele.tagGroupId;
            reviewerList.tagGroupName = ele.tagGroupName;
            reviewerList.coiReviewCommentTagsId = ele.coiReviewCommentTagsId;
            this.reviewCommentDetails.coiReviewCommentDto.coiReviewCommentTag.push(reviewerList);
        });
    }

    private updateAttachmentDetails(attachmentsDetails: any) {
        attachmentsDetails.forEach(ele => {
            const fileList: any = {};
            fileList.fileName = ele.fileName;
            fileList.commentId = ele.commentId;
            fileList.attachmentId = ele.attachmentId;
            fileList.mimeType = ele.mimeType;
            fileList.fileDataId = ele.fileDataId;
            this.reviewCommentDetails.coiReviewCommentDto.coiReviewCommentAttachment.push(fileList);
        });
    }

    addReplayComment(event) {
        this.addOrUpdateComment(event.details, this.uploadedFile);
    }

    removeChidReviewComment(event) {
        const RELATION_INDEX = this.commentList[event.index].reply.findIndex(ele => ele.commentId === event.commentId);
        if (RELATION_INDEX !== -1) {
            this.commentList[event.index].reply.splice(RELATION_INDEX, 1);
        }
    }

    loadReviewerCommentBody(details) {
        const REQ_BODY = {
            personId: null,
            disclosureId: details.disclosureId,
            coiSubSectionsId: details.coiSubSectionsId,
            coiSectionsTypeCode: details.coiSectionsTypeCode || null,
            componentSubRefId: details.componentSubRefId || null,
            sort: "desc"
        }
        return REQ_BODY;
    }

    
}