import { Component, ElementRef, EventEmitter, Input, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { CommentConfiguration, CompleterOptions, CommentFetch, CoiReviewComment } from './review-comments-interface';
import { ReviewCommentsService } from './review-comments.service';
import { COMMON_ERROR_TOAST_MSG, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { topSlideInOut } from '../../common/utilities/animations';
import {EDITOR_CONFIURATION} from '../../../../../fibi/src/app/app-constants';
import * as DecoupledEditor from '@ckeditor/ckeditor5-build-decoupled-document';
import { hideModal, openModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { environment } from '../../../environments/environment';
import { Router } from '@angular/router';
import { openCoiSlider } from '../../common/utilities/custom-utilities';


@Component({
    selector: 'app-review-comments-slider',
    templateUrl: './review-comments-slider.component.html',
    styleUrls: ['./review-comments-slider.component.scss'],
    providers: [ReviewCommentsService],
    animations: [topSlideInOut]
})
export class ReviewCommentsSliderComponent implements OnInit, OnDestroy {

    headerName = ''
    @Output() closePage: EventEmitter<any> = new EventEmitter<any>();
    @Input() disclosureDetails: any;
    @Input() disclosureType: any = "";
    @Input() projectDetails: any = null;
    @Input() selectedProject: any;
    @Input() reviewList: any = null;
    @Input() isViewMode = false;
    isSaving = false;
    $subscriptions: Subscription[] = [];
    isReadMore = false;
    reviewCommentDetails: CoiReviewComment = new CoiReviewComment();
    reviewTypeList: any;
    selectedProjectDetails:any = {};
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
    noFileChosen = false;
    multipleFile = false;
    public Editor = DecoupledEditor;
    editorConfig = EDITOR_CONFIURATION;
    selectedAttachmentDescription = [];
    selectedAttachmentType: any[] = [];
    selectedAttachmentStatus: any[] = [];
    selectedKeyPersonnel: any[] = [];
    statusSelected: boolean;
    coiFinancialEntityDetails: any[] = [];
    commentConfiguration: CommentConfiguration = new CommentConfiguration();
    projectList: any = [];
    activeProject = 1;
    projectRelations: any = [];
    sectionSearchOptions: CompleterOptions = new CompleterOptions();
    clearSectionField: String;
    subSectionSearchOptions: CompleterOptions = new CompleterOptions();
    clearSubSectionField: String;
    childSubSectionSelectOptions: CompleterOptions = new CompleterOptions();
    childSubSectionFiled: String;
    selectedReviewType1: any = {};
    searchPlaceHolder = '';
    coiSubSectionsTitle = '';
    personnelAttachTypes: any = [];
    attachmentWarningMsg = null;
    deployMap = environment.deployUrl;
    sfiStatus: any;
    reviewCommentType: any = [
        { code: 1, description: 'Public', isShowForAdmin: true, isShowForReviewer: true, isShowForReporter: true },
        { code: 2, description: 'Reviewer only', isShowForAdmin: true, isShowForReviewer: false, isShowForReporter: false },
        { code: 3, description: 'Admin only', isShowForAdmin: false, isShowForReviewer: true, isShowForReporter: true },
        { code: 4, description: 'Reporter only', isShowForAdmin: true, isShowForReviewer: false, isShowForReporter: false }
    ];
    selectedCommentType: any = null;
    componentSubRefTitle = '';
    coiSectionType = null;
    isUserAdmin = false;
    isUserReviewer = false;
    isUserReporter = false;
    subSectionTitle: string;
    subSectionId: any;
    showAddComment = false;
    showSlider = false;
    sliderElementId: any;
    imgUrl = this.deployMap + 'assets/images/close-black.svg';
    @ViewChild('reviewCommentsOverlay', { static: true }) reviewCommentsOverlay: ElementRef;

    constructor(
        public commonService: CommonService, private _reviewCommentsService: ReviewCommentsService,
        private _router: Router
    ) { }

    ngOnInit() {
        this.getReviewerActionDetails();
        // this.loadSectionsTypeCode();
        this.viewSlider();
    }

    viewSlider() {
        this.showSlider = true;
        this.sliderElementId = `review-comments-slider-${this.reviewCommentDetails.componentTypeCode}`;
        setTimeout(() => {
            openCoiSlider(this.sliderElementId);
        });
    }

    validateSliderClose() {
        setTimeout(() => {
            this.showSlider = false;
            this.sliderElementId = '';
            this.closePage.emit();
		}, 500);
	}

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    openConformationModal() {
        document.getElementById('review-comments-confirmation-modal-trigger-btn').click();
    }

    // validateSliderClose() {
    //     this.isCloseSlider = true;
    //     (this.isChangesInField) ? this.openConformationModal() : this.closeReviewSlider();
    // }

    closeReviewSlider() {
        const slider = document.querySelector('.slider-base');
        slider.classList.remove('slider-opened');
        // this.addBodyScroll();
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

    // loadSectionsTypeCode() {
    //     if(this.disclosureType=='COI') {
    //         this.$subscriptions.push(this._reviewCommentsService.getSectionsTypeCode({
    //             personId: this.disclosureDetails.personId,
    //             disclosureId: this.disclosureDetails.disclosureId
    //         }).subscribe((res: any) => {
    //             this.reviewTypeList = res;
    //             this.sectionSearchOptions = this.setCompleterOptions(this.reviewTypeList.coiSectionsTypeList, 'description');
    //         }));
    //     }
    // }

    fileDrop(event) {
        if (event) {
            this.uploadedFile.push(event[0]);
            this.selectedAttachmentType.push(null);
            this.isChangesInField = true;
        }

    }

    updateAddAttachmentDetails(files, index) {
        this.uploadedFile.push(files[index]);
        this.selectedAttachmentType[this.uploadedFile.length - 1] = null;
        this.selectedAttachmentDescription[this.uploadedFile.length - 1] = '';
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
                this.assignAdminMap.clear();
                this.clearAdminGroupField = new String('true');
                this.isChangesInField = true;
            }
        }
    }


    deleteCoiCommentAssignee(tagDetails, index) {
        if (this.isEditComment && tagDetails.coiReviewCommentTagsId) {
            this.$subscriptions.push(this._reviewCommentsService.deleteCOIAssignee(tagDetails.coiReviewCommentTagsId).subscribe(res => {
                this.reviewCommentDetails.commentTags.splice(index, 1);
                this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Assignee removed successfully.')
            }, err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG)
            }));
        } else {
            this.reviewCommentDetails.commentTags.splice(index, 1);
        }
    }

    addCommentsDetails() {
        if (!this.validateComment()) {
            this.addOrUpdateComment(this.reviewCommentDetails, this.uploadedFile);
        }
    }


    private addOrUpdateComment(details, file, showAddComment = false) {
        details.moduleCode = this.disclosureType == 'OPA' ? 23 : 8;
        if(this.reviewCommentDetails.componentTypeCode != '10') {
            this.$subscriptions.push(this._reviewCommentsService.addCOIReviewComment(details).subscribe((res: any) => {
                this.cancelOrClearCommentsDetails();
                if (details.parentCommentId) {
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Reply comment added successfully');
                } else {
                    this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Review comments added successfully');
                }
                this.showAddComment = showAddComment;
            }, error => {
                this.commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
            }));
        }
        if(this.reviewCommentDetails.componentTypeCode == '10') {
            this.$subscriptions.push(this._reviewCommentsService.addOPAReviewComment(details).subscribe((res: any) => {
                this.cancelOrClearCommentsDetails();
                this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Review comments added successfully');
            }, error => {
                this.commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
            }));
        }
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
        return this.reviewCommentDetails.commentTags.some((element) =>
            type === 'ADMINISTRATOR' ? element.tagPersonId === event.personId : element.tagGroupId === event.adminGroupId);
    }

    getCoiReviewComments(REQ_BODY) {
    if(REQ_BODY.componentTypeCode != '10') {
        this.$subscriptions.push(this._reviewCommentsService.getCoiReviewComments(REQ_BODY).subscribe((res: any) => {
            this.commentList = res;
            if (this._reviewCommentsService.editParentCommentId) {
                this._reviewCommentsService.editParentCommentId = null;
            }
        }));
    }
    if(REQ_BODY.componentTypeCode == '10') {
        this.$subscriptions.push(this._reviewCommentsService.getOPAReviewComments(REQ_BODY).subscribe((res: any) => {
            this.commentList = res;
            if (this._reviewCommentsService.editParentCommentId) {
                this._reviewCommentsService.editParentCommentId = null;
            }
        }));
    }
    }

    getReviewerActionDetails() {
        this.$subscriptions.push(this.commonService.$commentConfigurationDetails.subscribe((res: any) => {
            if(res) {
                this.reviewCommentDetails.documentOwnerPersonId = res.documentOwnerPersonId;
                this.reviewCommentDetails.moduleItemKey = this.disclosureType === 'COI' ? this.disclosureDetails.disclosureId : this.disclosureDetails.opaDisclosureId;
                this.reviewCommentDetails.componentTypeCode = res.componentTypeCode;
                if (['4', '5', '6', '8', '11'].includes(this.reviewCommentDetails.componentTypeCode)) {
                    this.headerName = res.headerName;
                    this.reviewCommentDetails.subModuleItemKey = res.subModuleItemKey;
                    this.reviewCommentDetails.subModuleItemNumber = res.subModuleItemNumber || null;
                    this.coiSubSectionsTitle = res.coiSubSectionsTitle;
                    this.selectedProjectDetails = res.selectedProject;
                    this.setLocalVariableValue(res)
                }
                if (this.reviewCommentDetails.componentTypeCode == 10) {
                    this.reviewCommentDetails.formBuilderComponentId = res.formBuilderComponentId;
                    this.reviewCommentDetails.formBuilderId = res.formBuilderId;
                    this.reviewCommentDetails.formBuilderSectionId = res.formBuilderSectionId;
                    this.coiSubSectionsTitle = res.headerName;
                }
                this.getCoiReviewComments(this.loadReviewerCommentBody(res));
            }
        }));
    }

    setLocalVariableValue(type) {
        if(type.sfiStatus) {
            this.sfiStatus = type.sfiStatus;
        }
        if (type.subSectionTitle) {
            this.subSectionTitle = type.subSectionTitle;
        }
        if(type.subSectionId) {
            this.subSectionId = type.subSectionId;
        }
    }

    deleteCoiCommentAttachments(item) {
        if (this.isEditComment) {
            this.$subscriptions.push(this._reviewCommentsService.deleteCOICommentAttachment({ attachmentId: item.attachmentId, fileDataId: item.fileDataId }).subscribe(res => {
                this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Attachment deleted successfully');
            }, error => {
                this.commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
            }));
        } else {
        }
    }

    validateComment() {
        this.mandatoryMap.clear();
        if (!this.reviewCommentDetails.comment) {
            this.mandatoryMap.set('comment', 'Please add comment');
        }
        return this.mandatoryMap.size === 0 ? false : true;
    }

    cancelOrClearCommentsDetails() {
        this.reviewCommentDetails = new CoiReviewComment();
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
        // this.commentList.splice(event, 1);
        if (!this.isCloseSlider) {
            this.getReviewerActionDetails();
        }
    }

    editReviewerParentComment(event) {
        this.isEditComment = true;
        this.reviewCommentDetails.documentOwnerPersonId = event.documentOwnerPersonId;
        this.reviewCommentDetails.commentId = event.commentId;
        this.reviewCommentDetails.isPrivate = event.isPrivate;
        this.reviewCommentDetails.comment = event.comment;
        this.reviewCommentDetails.subModuleItemKey = event.subModuleItemKey;
        this.reviewCommentDetails.componentTypeCode = event.componentTypeCode;
        this.reviewCommentDetails.moduleItemKey = this.reviewCommentDetails.moduleItemKey;
        this.updateAttachmentDetails(event.disclAttachments);
        this.updateAssigneeDetails(event.coiReviewCommentTag);
        this.addCommentsDetails();
    }

    private updateAssigneeDetails(assigneeDetails: any) {
        if(assigneeDetails && assigneeDetails.length) {
            assigneeDetails.forEach(ele => {
                const reviewerList: any = {};
                reviewerList.tagPersonId = ele.tagPersonId;
                reviewerList.tagPersonFullName = ele.tagPersonFullName;
                reviewerList.tagGroupId = ele.tagGroupId;
                reviewerList.tagGroupName = ele.tagGroupName;
                reviewerList.coiReviewCommentTagsId = ele.coiReviewCommentTagsId;
                this.reviewCommentDetails.commentTags.push(reviewerList);
            });
        }
    }

    private updateAttachmentDetails(attachmentsDetails: any) {
        if(attachmentsDetails && attachmentsDetails.length) {
            attachmentsDetails.forEach(ele => {
                const fileList: any = {};
                fileList.fileName = ele.fileName;
                fileList.commentId = ele.commentId;
                fileList.attachmentId = ele.attachmentId;
                fileList.mimeType = ele.mimeType;
                fileList.fileDataId = ele.fileDataId;
            });
        }
    }

    addReplayComment(event) {
        this.addOrUpdateComment(event.details, this.uploadedFile, this.showAddComment);
    }

    removeChidReviewComment(event) {
        // const RELATION_INDEX = this.commentList[event.index].childComments.findIndex(ele => ele.commentId === event.commentId);
        // if (RELATION_INDEX !== -1) {
        //     this.commentList[event.index].childComments.splice(RELATION_INDEX, 1);
        // }
        this.getReviewerActionDetails();
    }

    loadReviewerCommentBody(details) {
        const REQ_BODY = new CommentFetch();
        REQ_BODY.componentTypeCode = ['3', '9'].includes(details.componentTypeCode) ? null : details.componentTypeCode;
        REQ_BODY.moduleItemKey = this.disclosureType === 'COI' ? this.disclosureDetails.disclosureId : this.disclosureDetails.opaDisclosureId;
        REQ_BODY.moduleItemNumber = this.disclosureDetails.disclosureNumber;
        REQ_BODY.parentCommentId = this.disclosureDetails.parentCommentId || null;
        REQ_BODY.subModuleCode = details.subModuleCode || null;
        REQ_BODY.subModuleItemKey = details.subModuleItemKey || null;
        REQ_BODY.subModuleItemNumber = details.subModuleItemNumber || null;
        REQ_BODY.formBuilderComponentId = details.formBuilderComponentId || null;
        REQ_BODY.formBuilderId = details.formBuilderId || null;
        REQ_BODY.formBuilderSectionId = details.formBuilderSectionId || null;
        REQ_BODY.moduleCode = this.disclosureType === 'COI' ? 8 : 23;
        REQ_BODY.isSectionDetailsNeeded = ['3', '9'].includes(details.componentTypeCode);
        return REQ_BODY;
    }

    addToAttachment() {
        if (this.checkMandatoryFilled()) {
        this.uploadedFile.forEach(element => {
            const attachment = {
                fileName: element.name,
                commentId: null,
                attachmentId: null,
                mimeType: element.type,
            }
        });
    }
        this.dismissAttachmentModal();

    }

    dismissAttachmentModal() {
        this.mandatoryMap.clear();
        hideModal('add-attachment-modal');
        this.selectedAttachmentDescription = null;
    }


    attachmentModal() {
        if (!this.personnelAttachTypes?.length) {
            this.disclosureAttachmentTypes();
        }
        this.uploadedFile = [];
        this.noFileChosen = false;
        this.multipleFile = false;
        this.selectedAttachmentType = [];
        this.selectedAttachmentDescription = [];
        this.selectedAttachmentStatus = [];
    }

    public onReady(editor) {
        editor.ui.getEditableElement().parentElement.insertBefore(
            editor.ui.view.toolbar.element,
            editor.ui.getEditableElement()
        );
    }

    deleteFromUploadedFileList(index) {
        this.selectedAttachmentType.splice(index, 1);
        this.selectedAttachmentDescription.splice(index, 1);
        this.selectedAttachmentStatus.splice(index, 1);
        this.uploadedFile.splice(index, 1);
    }

    private setCompleterOptions(array, field) {
        return {
            arrayList: array,
            contextField: field,
            filterFields: field,
            formatString: field,
            defaultValue: ''
        };
    }

    setSubSectionOptions(typeCode) {
        if (typeCode === '4') {
            this.searchPlaceHolder = 'Search for Questionnaire';
            this.subSectionSearchOptions = this.setCompleterOptions(this.reviewTypeList.questionnaireDataBus.applicableQuestionnaire, 'QUESTIONNAIRE_LABEL')
        }
        if (typeCode === '5') {
            this.searchPlaceHolder = 'Search for SFI';
            this.subSectionSearchOptions = this.setOptionsForEntity(this.reviewTypeList.personEntities);
        }
        if (typeCode === '6') {
            this.searchPlaceHolder = 'Search for Project Relationship';
            this.subSectionSearchOptions = this.setCompleterOptions(this.reviewTypeList.projectList, 'title');
        }
    }

    setOptionsForEntity(array) {
        const entity = array.map(e => {return {personEntityId : e.personEntityId, entityName: e.coiEntity.entityName}});
        return this.setCompleterOptions(entity, 'entityName');
    }

    getSubSectionId(event) {
        if (this.reviewCommentDetails?.componentTypeCode === '4') {
            return event?.QUESTIONNAIRE_ID;
        }
        if (this.reviewCommentDetails?.componentTypeCode === '5') {
            return event.personEntityId;
        }
        if (this.reviewCommentDetails?.componentTypeCode === '6') {
            return event.moduleItemId;
        }
    }

    disclosureAttachmentTypes() {
            this.$subscriptions.push(this._reviewCommentsService.loadDisclAttachTypes().subscribe((data: any) => {
                    this.personnelAttachTypes = data;
                }, _err => this.commonService.showToast(HTTP_ERROR_STATUS, 'Fetching Attachment Types failed. Please try again.')));
        }

    checkMandatoryFilled() {
        this.attachmentWarningMsg = '';
        if (!this.uploadedFile?.length) {
            this.attachmentWarningMsg = '* No attachments added';
        } else {
            for (let uploadIndex = 0; uploadIndex < this.uploadedFile.length; uploadIndex++) {
                if (this.selectedAttachmentType[uploadIndex] === 'null' || this.selectedAttachmentType[uploadIndex] == null) {
                    this.attachmentWarningMsg = '* Please fill all the mandatory fields';
                    break;
                }
            }
        }
        return !this.attachmentWarningMsg;
    }

    isReviewComment(code) {
        return this.reviewCommentDetails.componentTypeCode == code
        && ((this.reviewCommentDetails.subModuleItemKey || (this.reviewCommentDetails.formBuilderSectionId && !this.reviewCommentDetails.formBuilderComponentId)));
    }

    openSFI(personEntityId) {
        this.validateSliderClose();
        this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: personEntityId, mode: 'view' } })
    }

    showTaskNavBar() {
        const slider = document.querySelector('.slider-base');
        slider.classList.add('slider-opened');
    }

    addBodyScroll() {
        document.getElementById('COI_SCROLL').classList.remove('overflow-hidden');
        document.getElementById('COI_SCROLL').classList.add('overflow-y-scroll');
    }

    toggleAddCommentBox(): void {
        this.showAddComment = !this.showAddComment;
    }

    
    redirectToProjectDetails(): void {
        const { documentNumber, projectId, projectTypeCode } = this.selectedProjectDetails || {};
        this.commonService.redirectToProjectDetails(projectTypeCode, (documentNumber || projectId));
    }
}
