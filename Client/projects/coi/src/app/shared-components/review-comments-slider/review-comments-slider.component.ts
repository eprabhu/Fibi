import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output } from '@angular/core';
import { CommonService } from '../../common/services/common.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription } from 'rxjs';
import { closeSlider, openSlider } from '../../common/utilities/custom-utilities';
import { CommentConfiguration, RO, ReviewComments, CompleterOptions } from './review-comments-interface';
import { ReviewCommentsService } from './review-comments.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { topSlideInOut } from '../../common/utilities/animations';
import {EDITOR_CONFIURATION} from '../../../../../fibi/src/app/app-constants';
import * as DecoupledEditor from '@ckeditor/ckeditor5-build-decoupled-document';
import { hideModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { DataStoreService } from '../../disclosure/services/data-store.service';
import { CoiService } from '../../disclosure/services/coi.service';
import { environment } from '../../../environments/environment';


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
    @Input() disclosureDetails: any;
    @Input() projectDetails: any;
    @Input() selectedProject: any;
    @Input() reviewList: any;
    isSaving = false;
    $subscriptions: Subscription[] = [];
    isReadMore = false;
    reviewCommentDetails: ReviewComments = new ReviewComments();
    reviewTypeList: any;
    moduleCode:any;
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

    constructor(
        public commonService: CommonService, private _reviewCommentsService: ReviewCommentsService, 
        public _dataStore: DataStoreService, public coiService: CoiService
    ) { }

    ngOnInit() {
        this.getPermission();
        this.getReviewerActionDetails();
        this.loadSectionsTypeCode();
        this.getAdminDetails();
        setTimeout(() => {
            openSlider('review-comments-slider');
        });
    }

    getPermission() {
        this.isUserAdmin = this.coiService.isCOIAdministrator;
        this.isUserReviewer = this.reviewList.find(e => e.assigneePersonId === this.commonService?.getCurrentUserDetail('personId')) ? true : false;
        this.isUserReporter = this.disclosureDetails.personId === this.commonService?.getCurrentUserDetail('personId');
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
        this.$subscriptions.push(this._reviewCommentsService.getSectionsTypeCode({
            personId: this.disclosureDetails.personId,
            disclosureId: this.disclosureDetails.disclosureId
        }).subscribe((res: any) => {
            this.reviewTypeList = res;
            this.sectionSearchOptions = this.setCompleterOptions(this.reviewTypeList.coiSectionsTypeList, 'description');
            this.setDefaultOptions();
        }));
    }

    getCoiSectionType() {
        if (this.reviewCommentDetails?.coiReviewCommentDto.componentSubRefId) {
            return 'Relationship between project';
        } else if (this.reviewCommentDetails?.coiReviewCommentDto.coiSectionsTypeCode == '6') {
            return 'Project';
        } else {
            return this.reviewTypeList.coiSectionsTypeList.find(e=> e.coiSectionsTypeCode == this.reviewCommentDetails?.coiReviewCommentDto.coiSectionsTypeCode)?.description;
        }
    }

    setDefaultOptions() {
        this.coiSectionType = this.getCoiSectionType();
        this.sectionSearchOptions.defaultValue = this.coiSectionType ? this.coiSectionType : '';
        this.setSubSectionOptions(this.reviewCommentDetails?.coiReviewCommentDto.coiSectionsTypeCode);
        this.subSectionSearchOptions.defaultValue = this.coiSubSectionsTitle;
        let COI_CHILD_SECTION_OPTIONS;
        if (this.reviewCommentDetails?.coiReviewCommentDto.coiSectionsTypeCode == '6' && this.reviewCommentDetails?.coiReviewCommentDto.coiSubSectionsId) {
            COI_CHILD_SECTION_OPTIONS = this.reviewTypeList.projectList.find(e=> e.moduleItemId == this.reviewCommentDetails?.coiReviewCommentDto.coiSubSectionsId);
            this.childSubSectionSelectOptions = this.setOptionsForEntity(COI_CHILD_SECTION_OPTIONS?.coiDisclEntProjDetails || []);
        }
        if (this.reviewCommentDetails?.coiReviewCommentDto.componentSubRefId) {
            this.componentSubRefTitle = COI_CHILD_SECTION_OPTIONS?.coiDisclEntProjDetails.find(e=> e.personEntityId === this.reviewCommentDetails?.coiReviewCommentDto.componentSubRefId)?.coiEntity?.entityName;
            this.childSubSectionSelectOptions.defaultValue = this.componentSubRefTitle;
        }
    }

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
            this.setPersonCompleterOptions(data.persons, 'fullName', this.adminSearchOptions);
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

    private setPersonCompleterOptions(arrayList: any, searchShowField: string, searchOption: any = null) {
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
                this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Assignee removed successfully.')
            }, err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.')
            }));
        } else {
            this.reviewCommentDetails.coiReviewCommentDto.coiReviewCommentTag.splice(index, 1);
        }
    }

    addCommentsDetails() {
        if (!this.validateComment()) {
            // this.reviewCommentDetails.coiReviewCommentDto.isAdmin = this.selectedCommentType.isShowForAdmin;
            // this.reviewCommentDetails.coiReviewCommentDto.isReviewer = this.selectedCommentType.isShowForReviewer;
            // this.reviewCommentDetails.coiReviewCommentDto.isReporter = this.selectedCommentType.isShowForReporter;
            this.addOrUpdateComment(this.reviewCommentDetails, this.uploadedFile);
        }
    }


    private addOrUpdateComment(details, file) {
        this.$subscriptions.push(this._reviewCommentsService.addCOIReviewComment(details, file).subscribe((res: any) => {
            this.getCoiReviewComments(this.loadReviewerCommentBody(res));
            this.cancelOrClearCommentsDetails();
            this.commonService.showToast(HTTP_SUCCESS_STATUS, 'General review comment added Successfully');
        }, error => {
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
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
        this.$subscriptions.push(this.commonService.$commentConfigurationDetails.subscribe((res: any) => {
            this.reviewCommentDetails.documentOwnerPersonId = res.documentOwnerPersonId;
            this.reviewCommentDetails.coiReviewCommentDto.disclosureId = res.disclosureId;
            this.reviewCommentDetails.coiReviewCommentDto.coiSectionsTypeCode = res.coiSectionsTypeCode;
            if (['4', '5', '6'].includes(this.reviewCommentDetails.coiReviewCommentDto.coiSectionsTypeCode)) {
                this.headerName = res.headerName;
                this.reviewCommentDetails.coiReviewCommentDto.coiSubSectionsId = res.coiSubSectionsId;
                this.coiSubSectionsTitle = res.coiSubSectionsTitle;
                this.moduleCode = res.moduleCode;
                this.reviewCommentDetails.coiReviewCommentDto.componentSubRefId = res.componentSubRefId;
                if(res.sfiStatus) {
                    this.sfiStatus = res.sfiStatus;
                }
            }
            this.getCoiReviewComments(this.loadReviewerCommentBody(res));
        }));
    }

    deleteCoiCommentAttachments(item, index) {
        if (this.isEditComment) {
            this.$subscriptions.push(this._reviewCommentsService.deleteCOICommentAttachment({ attachmentId: item.attachmentId, fileDataId: item.fileDataId }).subscribe(res => {
                this.reviewCommentDetails.coiReviewCommentDto.coiReviewCommentAttachment.splice(index, 1);
                this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Attachment deleted successfully');
            }, error => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
            }));
        } else {
            this.reviewCommentDetails.coiReviewCommentDto.coiReviewCommentAttachment.splice(index, 1);
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

    getWarningClass(typeCode): string {
        switch (typeCode) {
            case '1':
                return 'invalid';
            case '2':
                return 'medium-risk';
            case '3':
                return 'low-risk';
            default:
                return '';
        }
    }

    getDisclosureTitleName(fcoiTypeCode: any): string {
		switch (fcoiTypeCode) {
			case '1':
				return 'FCOI';
            default:
                    return 'Project';
		}
	}

	getColorBadges(disclosure): string {
        if(disclosure.fcoiTypeCode == 1) {
            return 'bg-fcoi-clip'
        } else {
            return 'project-background'
        }
	}
    
    modalHeader(projectDetails): string {
		return `# ${projectDetails.moduleCode == '3' ? projectDetails.moduleItemId : projectDetails.moduleItemKey} - ${projectDetails?.title}`;
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
            this.reviewCommentDetails.coiReviewCommentDto.coiReviewCommentAttachment.push(attachment);
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

    sectionSelect(event): void {
        if(event) {
            this.reviewCommentDetails.coiReviewCommentDto.coiSectionsTypeCode = event.coiSectionsTypeCode;
            this.setSubSectionOptions(this.reviewCommentDetails?.coiReviewCommentDto.coiSectionsTypeCode);
        } else {
            this.reviewCommentDetails.coiReviewCommentDto.coiSectionsTypeCode = null;
            this.reviewCommentDetails.coiReviewCommentDto.coiSubSectionsId = null;
        }
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

    subSectionSelect(event): void {
        this.reviewCommentDetails.coiReviewCommentDto.coiSubSectionsId = event ? this.getSubSectionId(event) : null;
        this.childSubSectionSelectOptions = event?.coiDisclEntProjDetails ? this.setOptionsForEntity(event?.coiDisclEntProjDetails) : new CompleterOptions();
    }
    
    setComponentSubRefId(event): void {
        this.reviewCommentDetails.coiReviewCommentDto.componentSubRefId = event ? event.personEntityId : null;
    }

    getSubSectionId(event) {
        if (this.reviewCommentDetails?.coiReviewCommentDto.coiSectionsTypeCode === '4') {
            return event?.QUESTIONNAIRE_ID;
        }
        if (this.reviewCommentDetails?.coiReviewCommentDto.coiSectionsTypeCode === '5') {
            return event.personEntityId;
        }
        if (this.reviewCommentDetails?.coiReviewCommentDto.coiSectionsTypeCode === '6') {
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
        return this.reviewCommentDetails.coiReviewCommentDto.coiSectionsTypeCode == code
        && this.reviewCommentDetails.coiReviewCommentDto.coiSubSectionsId;
    }
 

}