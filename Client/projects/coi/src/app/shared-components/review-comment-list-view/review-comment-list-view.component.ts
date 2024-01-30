import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output } from '@angular/core';
import { CoiReviewComment, ReviewComments } from '../review-comments-slider/review-comments-interface';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { ReviewCommentsService } from '../review-comments-slider/review-comments.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { deepCloneObject, fileDownloader, isEmptyObject } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { EDITOR_CONFIURATION } from '../../../../../fibi/src/app/app-constants';
import * as DecoupledEditor from '@ckeditor/ckeditor5-build-decoupled-document';

@Component({
	selector: 'app-review-comment-list-view',
	templateUrl: './review-comment-list-view.component.html',
	styleUrls: ['./review-comment-list-view.component.scss']
})
export class ReviewCommentListViewComponent implements OnInit, OnDestroy, OnChanges {

	@Input() commentReviewerList: any = [];
	@Input() reviewTypeList: any = [];
	@Input() disclosureDetails: any;
	@Input() selectedReviewType:any;
	@Input() disclosureType:any;
	@Input() reviewCommentDetails:any;
	@Input() isHeaderNeeded:any = false;
	@Output() deleteReviewComment: EventEmitter<any> = new EventEmitter<any>();
	@Output() editReviewParentComment: EventEmitter<any> = new EventEmitter<any>();
	@Output() emitReplayCommentDetails: EventEmitter<any> = new EventEmitter<any>();
	@Output() deleteChidReviewComment: EventEmitter<any> = new EventEmitter<any>();
	public Editor = DecoupledEditor;
    editorConfig = EDITOR_CONFIURATION;
	isEditComment = false;
	isEditParentComment = false;
	commentDetails: CoiReviewComment = new CoiReviewComment();
	$subscriptions: Subscription[] = [];
	isReplyComment = false;
	mandatoryMap = new Map();
	selectedCommentIdList:any = [];
	isShowReplyComment = false;
	readMoreCommentIdList:any = [];
	isReadMore = false;
	groupedCommentsList: any;

	constructor(private _commonService: CommonService, public reviewCommentsService: ReviewCommentsService) { }

	ngOnInit() {}

	ngOnChanges() {
		setTimeout(() => {
		this.groupedCommentsList = [];
		if(this.commentReviewerList && this.commentReviewerList.length) {
			this.groupedCommentsList = this.disclosureType === 'COI' ?
			this.groupBy(deepCloneObject(this.commentReviewerList), 'subModuleItemKey' ) :
			this.opaGroupBy(deepCloneObject(this.commentReviewerList), 'formBuilderSectionId' , 'subModuleItemKey');
		}
	}, 300);
	}

	ngOnDestroy() {
		subscriptionHandler(this.$subscriptions);
	}

	groupBy(jsonData, key) {
        return jsonData.reduce((relationsTypeGroup, item) => {
            (relationsTypeGroup[item[key]] = relationsTypeGroup[item[key]] || []).push(item);
            return relationsTypeGroup;
        }, {});
    }

	opaGroupBy(jsonData, formKey, subsectionKey) {
        return jsonData.reduce((relationsTypeGroup, item) => {
			let key = item[formKey] != null ? formKey : item[subsectionKey] != null ? subsectionKey : formKey;
			(relationsTypeGroup[item[key]] = relationsTypeGroup[item[key]] || []).push(item);
            return relationsTypeGroup;
        }, {});
    }

	deleteComment(details, index) {
		if (this.reviewCommentDetails.componentTypeCode != '10') {
			this.$subscriptions.push(this.reviewCommentsService.deleteReviewComments(details.commentId, details.moduleCode).subscribe
			((res: any) => {
				details.parentCommentId ? this.deleteReplyComment(details,index) : this.deleteReviewComment.emit(index);
				this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Comment deleted successfully');
				this.isShowReplyComment = false;
			}, error => {
				this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.')
			}));
		}
		if (this.reviewCommentDetails.componentTypeCode == '10') {
			this.$subscriptions.push(this.reviewCommentsService.deleteFormBuilderReviewComments(details.commentId, details.moduleCode).subscribe
			((res: any) => {
				details.parentCommentId ? this.deleteReplyComment(details,index) : this.deleteReviewComment.emit(index);
				this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Comment deleted successfully');
				this.isShowReplyComment = false;
			}, error => {
				this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.')
			}));
		}	
	}

	editParentComment(details, index) {
		if (!this.reviewCommentsService.isEditParentComment && !this.isEditComment && !this.isReplyComment) {
			this.reviewCommentsService.isEditParentComment = true;
			this.reviewCommentsService.editParentCommentId = details.commentId;
		}
	}

	addEditComment(list) {
		this.editReviewParentComment.emit(list);
	}

	replyComment(commentDetails, index) {
		if(!this.isReplyComment && !this.reviewCommentsService.isEditParentComment && !this.isEditComment) {
			this.getReviewerActionDetails();
			this.commentDetails.parentCommentId = commentDetails.commentId;
			this.isReplyComment = true;
		}

	}

	getReviewerActionDetails() {
		this.$subscriptions.push(this._commonService.$commentConfigurationDetails.subscribe((res: any) => {
			this.commentDetails.documentOwnerPersonId = res.documentOwnerPersonId;
			this.commentDetails.moduleItemKey = this.disclosureDetails.disclosureId ||  this.disclosureDetails.opaDisclosureId;
			this.commentDetails.subModuleItemKey = res.subModuleItemKey;
			this.commentDetails.formBuilderId = res.formBuilderId;
			this.commentDetails.formBuilderSectionId = res.formBuilderSectionId;
			this.commentDetails.formBuilderComponentId = res.formBuilderComponentId;
			this.commentDetails.componentTypeCode = res.componentTypeCode;
		}));
	}

	editReplyComment(replayComment) {
		if( !this.isEditComment  &&  !this.reviewCommentsService.isEditParentComment && !this.isReplyComment) {
			this.isEditComment = true;
			this.commentDetails.moduleItemKey = replayComment.componentReferenceId
			this.commentDetails.commentId = replayComment.commentId;
			this.commentDetails.comment = replayComment.comment;
			this.commentDetails.parentCommentId = replayComment.parentCommentId;
			this.commentDetails.componentTypeCode = replayComment.componentTypeCode;
			this.commentDetails.subModuleItemKey = replayComment.subModuleItemKey;
			this.commentDetails.formBuilderId = replayComment.formBuilderId;
			this.commentDetails.formBuilderSectionId = replayComment.formBuilderSectionId;
			this.commentDetails.formBuilderComponentId = replayComment.formBuilderComponentId;
		}
	}

	deleteReplyComment(replayComment, index) {
		this.deleteChidReviewComment.emit({commentId:replayComment.commentId, index: index})
	}

	cancelOrClearCommentsDetails() {
		this.commentDetails = new CoiReviewComment();
		if (this.isEditComment) {
			this.isEditComment = false;
		}
		if(this.isReplyComment) {
			this.isReplyComment = false;
		}
		if (this.reviewCommentsService.isEditParentComment) {
            this.reviewCommentsService.isEditParentComment = false;
        }
        if (this.reviewCommentsService.editParentCommentId) {
            this.reviewCommentsService.editParentCommentId = null;
        }
		this.getReviewerActionDetails();
	}

	addReplayCommentsDetails() {
		if(!this.validateComment()) {
			this.commentDetails.moduleItemKey = this.disclosureDetails.disclosureId || this.disclosureDetails.opaDisclosureId;
            this.emitReplayCommentDetails.emit({details:this.commentDetails});
            this.cancelOrClearCommentsDetails();
            }
	}

	validateComment() {
        this.mandatoryMap.clear();
        if (this.commentDetails.comment === '' ) {
            this.mandatoryMap.set('comment', 'Please add comment');
        }
        return this.mandatoryMap.size === 0 ? false : true;
    }
	
	downloadAttachment(attachment) {
        this.$subscriptions.push(this.reviewCommentsService.downloadAttachment(attachment.attachmentId).subscribe(res => {
            fileDownloader(res,attachment.fileName);
            this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Attachment downloaded successfully');
        },error =>{
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.')
        }));
    }

	showReplyComment(details, actionType) {
		if (details?.childComments?.length) {
			if (actionType == 'SHOW') {
				this.selectedCommentIdList.push(details.commentId);
				this.isShowReplyComment = true;
			} else {
				const INDEX = this.selectedCommentIdList.findIndex(ele => ele === details.commentId);
				this.selectedCommentIdList.splice(INDEX,1);
				this.isShowReplyComment = this.selectedCommentIdList.length ?  true : false; 
			}
		}
	}

	public onReady(editor) {
        editor.ui.getEditableElement().parentElement.insertBefore(
            editor.ui.view.toolbar.element,
            editor.ui.getEditableElement()
        );
    }

	readMore(details,actionType) {
			if (actionType == 'MORE') {
				this.readMoreCommentIdList.push(details.commentId);
			} else {
				const INDEX = this.readMoreCommentIdList.findIndex(ele => ele === details.commentId);
				this.readMoreCommentIdList.splice(INDEX,1);
			}
	}

	getSectionName(valueArray) {
		let valueArrayRes = valueArray.find(ele => (ele.moduleSectionDetails && (ele.moduleSectionDetails.sectionName != null || ele.moduleSectionDetails.otherDetails !=null)));
		if (valueArrayRes && valueArrayRes.moduleSectionDetails?.sectionName) {
			return valueArrayRes.moduleSectionDetails?.sectionName;
		} else if (valueArrayRes && valueArrayRes.moduleSectionDetails?.otherDetails) {
			return valueArrayRes.moduleSectionDetails?.otherDetails?.location;
		} else {
			return 'General Comments';
		}
		// subsection.value[0]?.moduleSectionDetails ? subsection.value[0]?.moduleSectionDetails?.sectionName : 
	}

}
